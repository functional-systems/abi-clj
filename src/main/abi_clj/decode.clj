(ns abi-clj.decode
  (:require
   [abi-clj.utils :as utils.abi]
   [abi-clj.utils.hex :as utils.hex]
   [clojure.math :as math]
   [clojure.string :as str])
  (:import
   [java.lang Integer]
   [java.math BigInteger]))

(defn- remove-0x [data]
  (str/replace data #"^0x" ""))

(defn- extract-data-at
  ([data cursor] (extract-data-at data cursor 1))
  ([data cursor len] (subs (remove-0x data) cursor (+ cursor (* len 64)))))

(defn- decode-dispatch [{type :type}]
  (cond
    (re-matches #"^((uint|int|bool|address|bytes|tuple|string)(\d+)?)((\[\d*\])+)$" type) :vector
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
    ini-cursor :cursor
    ini-offset :offset
    :or {ini-cursor 0 ini-offset 0} :as item}]
  (let [data (remove-0x data)
        with-names? (every? :name components)
        dynamic? (utils.abi/item->is-dynamic? item)
        offset (if dynamic?
                 (param {:type "uint256" :data data :cursor ini-cursor})
                 0)
        ini-data-cursor (+ ini-offset (* 2 offset))

        res (loop [cursor ini-data-cursor component (first components) components (rest components) res []]
              (if component
                (let [size (utils.abi/item->size component)
                      dynamic? (utils.abi/item->is-dynamic? component)
                      decoded (param (assoc component :data data :cursor cursor :offset (if dynamic? ini-data-cursor cursor)))
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
    ini-cursor :cursor
    ini-offset :offset
    :or {ini-cursor 0 ini-offset 0} :as item}]
  (let [data (remove-0x data)
        match (re-matches #"^((uint|int|bool|address|bytes|tuple|string)(\d+)?)((\[\d*\])+)$" type)
        type (second match)
        dimension (nth match 4)
        rem-dim (re-matches #"((\[\d*\])+)(\[\d*\])$" dimension)
        conc-dim (when rem-dim (second rem-dim))
        size (utils.abi/item->size (assoc item :type (str type conc-dim)))
        dynamic? (utils.abi/item->is-dynamic? item)
        [len ini-data-cursor] (if dynamic?
                                (let [offset (param {:type "uint256" :data data :cursor ini-cursor})]
                                  [(param {:type "uint256" :data data :cursor (+ ini-offset (* 2 offset))})
                                   (+ ini-cursor (* 2 offset) 64)])

                                [(-> (last match)
                                     (str/replace #"\[|\]" "")
                                     Integer/parseInt)
                                 ini-cursor])]

    (->> (iterate #(+ size %) 0)
         (take len)
         (map #(let [dynamic? (utils.abi/item->is-dynamic? (assoc item :type (str type conc-dim)))]
                 (param (assoc item
                               :type (str type conc-dim)
                               :data data
                               :cursor (+ ini-data-cursor %)
                               :offset (if dynamic?
                                         ini-data-cursor
                                         (+ ini-data-cursor %))))))
         vec)))

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

(defn- to-utf-str [b] (String. b "UTF-8"))

(defmethod param :string
  [{data :data cursor :cursor ini-offset :offset :or {cursor 0 ini-offset 0}}]
  (let [data (remove-0x data)
        offset (param {:type "uint256" :data data :cursor cursor})
        len (param {:type "uint256" :data data :cursor (+ ini-offset (* 2 offset))})
        data-cursor (+ ini-offset (* 2 offset) 64)
        parts (math/ceil (/ len 64))]
    (->> (subs data data-cursor (+ data-cursor (* parts 64)))
         (partition-all 2)
         (map #(Integer/parseInt (str/join %) 16))
         (filter (complement #{0}))
         byte-array
         to-utf-str)))

(defmethod param :default [_] (throw (ex-info "Not implemented" {:causes :lazyness})))

(defn event
  [{abi-item :abi-item event :event}]
  (let [indexed (filter :indexed (:inputs abi-item))
        not-indexed (filter (comp not :indexed) (:inputs abi-item))]
    (param {:type "tuple" :components (concat indexed not-indexed) :data (utils.hex/concat (conj (vec (rest (:topics event)))
                                                                                                 (:data event)))})))

(defn function-result
  [{abi-item :abi-item data :data}]
  (let [first-item (first (:outputs abi-item))]
    (if (= 1 (count (:outputs abi-item)))
      (param (assoc first-item :data data))
      (param {:type "tuple" :components (:outputs abi-item) :data data}))))
