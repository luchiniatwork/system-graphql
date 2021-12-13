(ns system-lacinia.resolvers-util
  (:import (java.text SimpleDateFormat)
           (java.util TimeZone
                      UUID)))

(def ^:private df (doto (new SimpleDateFormat "yyyy-MM-dd'T'HH:mm:ss.SSSZ")
                    (.setTimeZone (TimeZone/getTimeZone "GMT"))))

(defn parse-date [str]
  (.parse df str))

(defn format-date [date]
  (.format df date))

(defn parse-uuid [str]
  (UUID/fromString str))

(defn stringify-uuid [uuid]
  (str uuid))

(defn ^:private find-in-mapping
  [side mapping k]
  (let [[source-fn target-fn xfn] (case side
                                    :a [first second #(nth % 2)]
                                    :b [second first #(nth % 3)])]
    (some->> mapping
             (filter #(= k (source-fn %)))
             first
             ((fn [i] [(target-fn i) (if (> (count i) 2) (xfn i) nil)])))))

(defn transform [side mapping x]
  (reduce (fn [m [k v]]
            (if-let [[new-k xfn] (find-in-mapping side mapping k)]
              (if xfn
                (assoc m new-k (xfn v))
                (assoc m new-k v))
              m))
          {} x))

(def transform-ab (partial transform :a))

(def transform-ba (partial transform :b))
