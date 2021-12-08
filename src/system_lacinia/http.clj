(ns system-lacinia.http
  (:require [io.pedestal.http :as http]
            [integrant.core :as ig]
            [taoensso.timbre :refer [debug info warn error fatal report]]))

(defmethod ig/init-key ::server [_ {:keys [base-service port] :as opts}]
  (info {:msg "Initializing Pedestal"
         :port port})
  (-> base-service
      (assoc ::http/port port
             ::http/host "0.0.0.0")
      http/create-server
      http/start))

(defmethod ig/halt-key! ::server [_ server]
  (info {:msg "Halting Pedestal"})
  (http/stop server))
