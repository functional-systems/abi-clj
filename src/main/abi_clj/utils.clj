(ns abi-clj.utils
  (:require
   [clojure.string :as str])
  (:import
   [java.lang Integer]))

(defn- dispatch-param [{type :type}]
  (cond
    (re-matches #"^((uint|int|bool|address|bytes|tuple|string)(\d+)?)((\[\d*\])+)$" type) :vector
    (= type "tuple")                   :tuple
    (= type "bool")                    :static
    (= type "address")                 :static
    (= type "string")                  :string
    (re-matches #"^uint.*" type)       :static
    (re-matches #"^int.*" type)        :static
    (re-matches #"^bytes(\d+)$" type)  :static
    (re-matches #"^bytes$" type)       :dbytes))

(defmulti item->is-dynamic? #'dispatch-param)

(defmethod item->is-dynamic? :vector
  [{type :type}]
  (let [match (re-matches #"^((uint|int|bool|address|bytes|tuple|string)(\d+)?)((\[\d*\])+)$" type)
        type (second match)
        dimension (nth match 4)
        rem-dim (re-matches #"((\[\d*\])+)(\[\d*\])$" dimension)
        conc-dim (when rem-dim (second rem-dim))
        dynamic? (= "[]" (last match))]
    (or dynamic? (item->is-dynamic? {:type (str type conc-dim)}))))

(defmethod item->is-dynamic? :tuple
  [{components :components}]
  (some item->is-dynamic? components))

(defmethod item->is-dynamic? :string
  [_] true)

(defmethod item->is-dynamic? :dbytes
  [_] true)

(defmethod item->is-dynamic? :default
  [_] false)

(defmulti item->size #'dispatch-param)

(defmethod item->size :tuple
  [{components :components :as item}]
  (if (item->is-dynamic? item)
    64
    (apply + (map item->size components))))

(defmethod item->size :vector
  [{type :type :as item}]
  (if (item->is-dynamic? item)
    64
    (let [match (re-seq #"\[(\d+)\]" type)
          type (re-matches #"^((uint|int|bool|address|bytes)(\d+)?).*$" type)]
      (apply * (item->size {:type (second type)}) (map (comp Integer/parseInt second) match)))))

(defmethod item->size :default [_] 64)

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

(defn- dispatch-item-signature [item] (if (#{"function" "event"} (:type item))
                                        "function"
                                        (:type item)))

(defmulti item->signature #'dispatch-item-signature)

(defmethod item->signature "function"
  [{inputs :inputs fname :name}]
  (str fname (item->human-readable {:type "tuple" :components (map #(dissoc % :name) inputs)})))

(defmethod item->signature :default [_] (throw (ex-info "Not implemented" {:causes :lazyness})))
