(ns abi-clj.utils
  (:require
   [clojure.string :as str])
  (:import
   [java.lang Integer]))

(defn- dispatch-param [{type :type}]
  (cond
    (re-matches #"^((uint|int|bool|address|bytes)(\d+)?)((\[\d+\])+)$" type) :vector
    (= type "tuple")                   :tuple
    (= type "bool")                    :static
    (= type "address")                 :static
    (= type "string")                  :string
    (re-matches #"^uint.*" type)       :static
    (re-matches #"^int.*" type)        :static
    (re-matches #"^bytes(\d+)$" type)  :static
    (re-matches #"^bytes$" type)       :dbytes))

(defmulti item->size #'dispatch-param)

(defmethod item->size :static [_] 64)

(defmethod item->size :tuple
  [{components :components}]
  (apply + (map item->size components)))

(defmethod item->size :vector
  [{type :type}]
  (let [match (re-seq #"\[(\d+)\]" type)
        type (re-matches #"^((uint|int|bool|address|bytes)(\d+)?).*$" type)]
    (apply * (item->size {:type (second type)}) (map (comp Integer/parseInt second) match))))

(defmulti item->human-readable :type)

(defmethod item->human-readable "function"
  [{inputs :inputs fname :name state :stateMutability outputs :outputs}]
  (str "function "
       fname
       (item->human-readable {:type "tuple" :components inputs})
       (when (and outputs (= state "view")) " view")
       (when outputs " returns ")
       (when outputs (item->human-readable {:type "tuple" :components outputs}))))

(defmethod item->human-readable "tuple"
  [{components :components name :name}]
  (let [with-name? (every? (comp seq :name) components)]
    (format (if name
              "(%s) %s"
              "(%s)")
            (str/join (if with-name? ", " ",") (map #(item->human-readable %) components))
            name)))

(defmethod item->human-readable :default
  [{type :type name :name}]
  (str type (when (seq name) " ") name))

(defn function-item->signature
  [{inputs :inputs fname :name}]
  (str fname (item->human-readable {:type "tuple" :components (map #(dissoc % :name) inputs)})))

(def event-item->signature function-item->signature) ;; its the same implementation
