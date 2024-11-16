(ns abi-clj.decode
  (:require
   [abi-clj.utils.hex :as utils.hex]
   [abi-clj.utils :as utils.abi]
   [clojure.string :as str])
  (:import
   [java.math BigInteger]
   [java.lang Integer]))

(defn- remove-0x [data]
  (str/replace data #"^0x" ""))

(defn- extract-data-at
  ([data cursor] (extract-data-at data cursor 1))
  ([data cursor len] (subs (remove-0x data) cursor (+ cursor (* len 64)))))

(defn- decode-dispatch [{type :type}]
  (cond
    (re-matches #"^((uint|int|bool|address|bytes|tuple)(\d+)?)((\[\d*\])+)$" type) :vector
    (= type "tuple")                   :tuple
    (= type "bool")                    :bool
    (= type "address")                 :address
    (= type "string")                  :string
    (re-matches #"^uint.*" type)       :unumber
    (re-matches #"^int.*" type)        :number
    (re-matches #"^bytes(\d+)$" type)  :bytes
    (re-matches #"^bytes$" type)       :dbytes))

(defmulti param #'decode-dispatch)

(defmethod param :tuple
  [{data :data
    components :components
    ini-cursor :cursor :or {ini-cursor 0}}]
  (let [data (remove-0x data)
        with-names? (every? :name components)
        res (loop [cursor ini-cursor component (first components) components (rest components) res []]
              (if component
                (let [size (utils.abi/item->size component)
                      decoded (param (assoc component :data data :cursor cursor))
                      append (if with-names?
                               [(keyword (:name component)) decoded] decoded)]
                  (recur (+ cursor size) (first components) (rest components) (conj res append)))
                res))]

    (if with-names?
      (into {} res)
      (vec res))))

(defmethod param :vector
  [{data :data
    type :type
    ini-cursor :cursor :or {ini-cursor 0} :as component}]
  (let [data (remove-0x data)
        match (re-matches #"^((uint|int|bool|address|bytes|tuple)(\d+)?)((\[\d*\])+)$" type)
        type (second match)
        dimension (nth match 4)
        rem-dim (re-matches #"((\[\d*\])+)(\[\d*\])$" dimension)
        conc-dim (when rem-dim (second rem-dim))
        size (utils.abi/item->size (assoc component :type (str type conc-dim)))
        dynamic? (= "[]" (last match))
        [len ini-cursor] (if dynamic?
                           (let [offset (param {:type "uint256" :data data :cursor ini-cursor})]
                             [(param {:type "uint256" :data data :cursor (+ ini-cursor (* 2 offset))})
                              (+ ini-cursor (* 2 offset) 64)])

                           [(-> (last match)
                                (str/replace #"\[|\]" "")
                                Integer/parseInt)
                            ini-cursor])]

    (->> (iterate #(+ size %) 0)
         (take len)
         (map #(param {:type (str type conc-dim) :data data :cursor (+ ini-cursor %)}))
         vec)))

(comment

  (def size 64)
  (def len 4)

  (->> (iterate #(+ size %) 0)
       (take len))

;;
  )

(defmethod param :unumber
  [{data :data cursor :cursor :or {cursor 0}}]
  (BigInteger. (extract-data-at data cursor) 16))

(defmethod param :number
  [{data :data cursor :cursor :or {cursor 0}}]
  (let [val (BigInteger. (extract-data-at data cursor) 16)
        max (.subtract (.shiftLeft (BigInteger. (str "1")) (- (* 32 8) 1)) (BigInteger. (str 1)))]
    (if (< val max) val
        (.subtract val (.shiftLeft (BigInteger. (str "1")) (* 32 8))))))

(defmethod param :address
  [{data :data cursor :cursor :or {cursor 0}}]
  (utils.hex/concat [(-> (extract-data-at data cursor)
                         (subs (- 64 40) 64))]))

(defmethod param :bool
  [{data :data cursor :cursor :or {cursor 0}}]
  (< 0 (BigInteger. (extract-data-at data cursor))))

(defmethod param :bytes
  [{data :data type :type cursor :cursor :or {cursor 0}}]
  (let [size (->> (re-matches #"^bytes(\d+)$" type)
                  second
                  Integer/parseInt)]
    (utils.hex/concat [(subs (extract-data-at data cursor) 0 (* size 2))])))

(defmethod param :default [_] (throw (ex-info "Not implemented" {:causes :lazyness})))

(defn event
  [{abi-item :abi-item event :event}]
  (param {:type "tuple" :components (:inputs abi-item) :data (utils.hex/concat (conj (vec (rest (:topics event)))
                                                                                     (:data event)))}))

(defn function-result
  [{abi-item :abi-item data :data}]
  (param {:type "tuple" :components (:outputs abi-item) :data data}))
