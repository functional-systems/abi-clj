(ns abi-clj.decode
  (:require
   [abi-clj.utils.hex :as utils.hex]
   [abi-clj.utils.abi :as utils.abi]
   [clojure.string :as str])
  (:import
   [java.math BigInteger]
   [java.lang Integer]))

(defn- decode-dispatch [{type :type}]
  (cond
    (re-matches #"^((uint|int|bool|address|bytes)(\d+)?)((\[\d+\])+)$" type) :vector
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
    components :components}]
  (let [value (str/replace data #"^0x" "")
        with-names? (every? :name components)
        res (loop [cursor 0 component (first components) components (rest components) res []]
              (if component
                (let [size (utils.abi/item->size component)
                      decoded (param (merge component {:data (utils.hex/concat [(subs value cursor (+ cursor size))])}))
                      append (if with-names?
                               [(keyword (:name component)) decoded]
                               decoded)]
                  (recur (+ cursor size) (first components) (rest components) (conj res append)))
                res))]

    (if with-names?
      (into {} res)
      (vec res))))

(defmethod param :vector
  [{data :data
    type :type}]
  (let [match (re-matches #"^((uint|int|bool|address|bytes)(\d+)?)((\[\d+\])+)$" type)
        type (second match)
        dimension (nth match 4)
        rem-dim (re-matches #"((\[\d+\])+)(\[\d+\])$" dimension)
        conc-dim (when rem-dim (second rem-dim))
        len (- (count data) 2)
        split (->> (nth match 5)
                   (re-matches #"^\[(\d+)\]")
                   second
                   Integer/parseInt
                   (/ len))]
    (->> (partition-all split (str/replace data #"^0x" ""))
         (map #(param {:type (str type conc-dim) :data (utils.hex/concat [(str/join %)])}))
         vec)))

(defmethod param :unumber
  [{data :data}]
  (BigInteger. (str/replace data #"^0x" "") 16))

(defmethod param :number
  [{data :data}]
  (let [val (BigInteger. (str/replace data #"^0x" "") 16)
        max (.subtract (.shiftLeft (BigInteger. (str "1")) (- (* 32 8) 1)) (BigInteger. (str 1)))]
    (if (< val max) val
        (.subtract val (.shiftLeft (BigInteger. (str "1")) (* 32 8))))))

(defmethod param :address
  [{data :data}]
  (utils.hex/concat [(-> data
                         (str/replace #"^0x" "")
                         (subs (- 64 40) 64))]))

(defmethod param :bool
  [{data :data}]
  (< 0 (BigInteger. (str/replace data #"^0x" ""))))

(defmethod param :bytes
  [{data :data type :type}]
  (let [size (->> (re-matches #"^bytes(\d+)$" type)
                  second
                  Integer/parseInt)]
    (subs data 0 (+ 2 (* size 2)))))

(defn event
  [{abi-item :abi-item event :event}]
  (param {:type "tuple" :components (:inputs abi-item) :data (utils.hex/concat (conj (vec (rest (:topics event)))
                                                                                     (:data event)))}))

(defn function-result
  [{abi-item :abi-item data :data}]
  (param {:type "tuple" :components (:outputs abi-item) :data data}))
