(ns system-lacinia.graphql
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [com.walmartlabs.lacinia.parser.schema :as lacinia-parser]
            [com.walmartlabs.lacinia.pedestal :as lacinia-pedestal]
            [com.walmartlabs.lacinia.pedestal2 :as lacinia-pedestal2]
            [com.walmartlabs.lacinia.schema :as lacinia-schema]
            [com.walmartlabs.lacinia.util :as lacinia-util]
            [hodur-engine.core :as hodur]
            [hodur-lacinia-schema.core :as hodur-lacinia]
            [integrant.core :as ig]
            [io.pedestal.http :as http]
            [jsonista.core :as json]
            [system-utils.correlation :refer [*correlation-id*]]
            [taoensso.timbre :refer [debug info warn error fatal report] :as timbre]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private schema-initializer
  [resource schema-fn]
  (-> resource
      io/resource
      hodur/init-path
      schema-fn))

(defmethod ig/init-key ::schema [_ {:keys [resource] :as opts}]
  (info {:msg "Initializing Hodur's GraphQL (Lacinia) Schema"
         :resource resource})
  (schema-initializer resource #(hodur-lacinia/schema % {:output :sdl})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interceptors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private log-interceptor
  {:name ::timbre-log
   :enter (fn [{:keys [request] :as context}]
            (let [{:keys [graphql-operation-name remote-addr]
                   :or {graphql-operation-name "<unnamed>"}} request
                  start (. System (nanoTime))]
              (info {:msg "GraphQL Request"
                     :from remote-addr
                     :graphql-operation graphql-operation-name})
              (assoc-in context [:request :profiling] {:start start})))
   :leave (fn [{:keys [request response] :as context}]
            (let [{:keys [graphql-operation-name remote-addr]
                   :or {graphql-operation-name "<unnamed>"}} request
                  {:keys [start]} (:profiling request)
                  elapsed (/ (double (- (. System (nanoTime)) start)) 1000000.0)]
              (info {:msg "GraphQL Response"
                     :to remote-addr
                     :graphql-operation graphql-operation-name
                     :elapsed-msecs elapsed
                     :status (:status response)
                     :errors (get-in response [:body :errors])}))
            context)})

(def ^:private correlation-id-interceptor
  {:name ::correlation-id
   :enter (fn [{:keys [request] :as context}]
            (let [uuid (or (get-in request [:headers "X-Correlation-ID"])
                           (str (java.util.UUID/randomUUID)))]
              (timbre/with-context {:correlation-id uuid}
                (-> context
                    (assoc-in [:request :correlation-id] uuid)
                    (update :bindings #(assoc % #'*correlation-id* uuid ))))))
   :leave (fn [{:keys [response] :as context}]
            (-> context
                (update-in [:response :headers] #(assoc % "X-Correlation-ID" *correlation-id*))))})

(defn ^:private augment-auth-error [ex]
  (let [{:keys [anomaly/category]} (ex-data ex)]
    (cond-> {}
      (and category (= "security.anomaly" (namespace category)))
      (assoc :status 401))))

(def ^:private error-handling-interceptor
  {:name ::error-handling-interceptor
   :error (fn [context ^Exception ex]
            (let [lacinia-ex (-> ex ex-data :exception)
                  cause-ex (.getCause lacinia-ex)
                  data (merge (ex-data lacinia-ex)
                              (ex-data cause-ex))
                  {:keys [status additional-headers msg]} (augment-auth-error lacinia-ex)
                  msg' (or (.getMessage lacinia-ex) msg)]
              (error (merge {:msg msg'} data))
              (assoc context :response
                     {:status (or status 500)
                      :body (json/write-value-as-string
                             {:errors [(merge {:message msg'}
                                              (dissoc data :status additional-headers))]})
                      :headers (merge {"Content-Type" "application/json"}
                                      additional-headers)})))
   :leave (fn [{:keys [response] :as context}]
            (let [errors (get-in response [:body :errors])
                  namespace-matcher (fn [target] #(= target (some-> % :extensions :anomaly/category namespace)))]
              ;; security errors/logic should go here
              (cond-> context
                #_(some (namespace-matcher "security.anomaly") errors)
                #_(assoc-in [:response :status] 401)

                #_(some (namespace-matcher "eventstore.anomaly") errors)
                #_(assoc-in [:response :status] 400)

                #_(some #(and ((namespace-matcher "eventstore.anomaly") %)
                              (re-matches #"^.*-not-found$" (-> % :extensions :anomaly/category name)))
                        errors)
                #_(assoc-in [:response :status] 404))))})

(defn ^:private create-principal-interceptor [{:keys [principal]}]
  {:name ::correlation-id
   :enter (fn [{:keys [request] :as context}]
            (let [^String auth-header (get-in request [:headers "authorization"])
                  token (when (and auth-header (s/starts-with? auth-header "Bearer "))
                          (.substring auth-header 7))]
              (assoc-in context
                        [:request :principal-session]
                        (principal token))))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ig/init-key ::compiled-schema [_ {:keys [sdl-schema resolvers entity-resolvers]}]
  (info {:msg "Initializing Compiled Lacinia Schema"
         :sdl-schema sdl-schema})
  (let [federation-settings (when entity-resolvers
                              {:federation {:entity-resolvers entity-resolvers}})]
    (-> sdl-schema
        (lacinia-parser/parse-schema federation-settings)
        (lacinia-util/inject-resolvers resolvers)
        lacinia-schema/compile)))


(defmethod ig/init-key ::log-interceptor [_ _]
  (info {:msg "Initializing Lacinia Log Interceptor"})
  {:interceptor log-interceptor
   :position :after
   :reference ::lacinia-pedestal2/inject-app-context})


(defmethod ig/init-key ::correlation-id-interceptor [_ _]
  (info {:msg "Initializing Lacinia Correlation ID Interceptor"})
  {:interceptor correlation-id-interceptor
   :position :before
   :reference ::lacinia-pedestal2/inject-app-context})


(defmethod ig/init-key ::error-handling-interceptor [_ _]
  (info {:msg "Initializing Lacinia Error Handling Interceptor"})
  {:interceptor error-handling-interceptor
   :position :after
   :reference ::correlation-id})


(defmethod ig/init-key ::principal-token-interceptor [_ {:keys [principal] :as opts}]
  (info {:msg "Initializing Lacinia Principal Token Interceptor"})
  {:interceptor (create-principal-interceptor opts)
   :position :before
   :reference ::lacinia-pedestal2/inject-app-context})


(defmethod ig/init-key ::interceptors [_ {:keys [compiled-schema interceptors] :as opts}]
  (info {:msg "Initializing Lacinia Interceptors"})
  (reduce (fn [a {:keys [interceptor position reference]}]
            (lacinia-pedestal/inject a interceptor position reference))
          (lacinia-pedestal2/default-interceptors compiled-schema nil)
          interceptors))


(defmethod ig/init-key ::routes [_ {:keys [graphiql? interceptors] :as opts}]
  (info {:msg "Initializing Lacinia Routes"
         :graphiql? graphiql? :qty-interceptors (count interceptors)})
  (debug {:msg "Interceptor list"
          :interceptors (map (fn [{:keys [name enter leave error]}]
                               {:name name})
                             interceptors)})
  (let [graphiql-routes
        (into #{["/ide" :get (lacinia-pedestal2/graphiql-ide-handler nil) :route-name ::graphiql-ide]}
              (lacinia-pedestal2/graphiql-asset-routes "/assets/graphiql"))]
    (cond-> #{["/api" :post interceptors :route-name ::graphql-api]}
      graphiql?
      (into graphiql-routes))))


(defmethod ig/init-key ::service [_ {:keys [graphiql? compiled-schema routes] :as opts}]
  (info {:msg "Initializing Lacinia Service"
         :graphiql? graphiql?})
  (cond-> {:env :dev
           ::http/allowed-origins (constantly true)
           ::http/routes routes
           ::http/type :jetty
           ::http/join? false}

    graphiql?
    lacinia-pedestal2/enable-graphiql

    :always
    (lacinia-pedestal2/enable-subscriptions compiled-schema nil)))
