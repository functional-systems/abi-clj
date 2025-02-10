(ns abi-clj.encode
  (:require
   [abi-clj.utils :as utils.abi]
   [abi-clj.utils.hex :as utils.hex]
   [clojure.string :as str])
  (:import
   [java.math BigInteger]
   [java.nio.charset StandardCharsets]
   [org.bouncycastle.util.encoders Hex]))

(defn- encode-dispatch [{type :type}]
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

(defmulti param #'encode-dispatch)

(defmethod param :tuple
  [{value :value
    components :components
    :as item}]
  (let [values (if (map? value)
                 (map (fn [item] (or (get value (keyword (:name item)))
                                     (get value (:name item))))
                      components)
                 value)
        pre-encoded (->> (zipmap components values)
                         (map (fn [[item value]] [item (param (assoc item
                                                                     :value value))])))
        static-cursor (apply + (->> pre-encoded
                                    (map (comp utils.abi/item->size first))))

        encoded (loop [dcursor static-cursor [item value] (first pre-encoded) pre-encoded (rest pre-encoded) static [] dynamic []]
                  (if item
                    (if (utils.abi/item->is-dynamic? item)
                      (let [cursor-delta (* (utils.hex/size value) 2)
                            offset (param {:type "uint256" :value (/ dcursor 2)})]
                        (recur (+ dcursor cursor-delta) (first pre-encoded) (rest pre-encoded) (conj static offset) (conj dynamic value)))
                      (recur dcursor (first pre-encoded) (rest pre-encoded) (conj static value) dynamic))
                    (utils.hex/concat static dynamic)))]
    encoded))

(defmethod param :vector
  [{value :value
    type  :type
    :as item}]
  (let [match (re-matches #"^((uint|int|bool|address|bytes|tuple|string)(\d+)?)((\[\d*\])+)$" type)
        type (second match)
        dimension (nth match 4)
        rem-dim (re-matches #"((\[\d+\])+)(\[\d+\])$" dimension)
        conc-dim (when rem-dim (second rem-dim))
        dynamic? (utils.abi/item->is-dynamic? item)
        child-item (assoc item :type (str type conc-dim))
        dynamic-child-item? (utils.abi/item->is-dynamic? child-item)
        len (count value)
        ini-cursor (* len 64)
        encoded (if dynamic-child-item?
                  (loop [dcursor ini-cursor v (first value) values (rest value) static [] dynamic []]
                    (if v
                      (let [encoded (param (assoc child-item :value v))
                            cursor-delta (* (utils.hex/size encoded) 2)
                            offset (param {:type "uint256" :value (/ dcursor 2)})]
                        (recur (+ dcursor cursor-delta) (first values) (rest values) (conj static offset) (conj dynamic encoded)))
                      (utils.hex/concat static dynamic)))
                  (->> value
                       (map #(param (assoc child-item :value %)))
                       utils.hex/concat))]
    (if dynamic?
      (utils.hex/concat [(param {:type "uint256" :value len}) encoded])
      encoded)))

(defmethod param :bool
  [{value :value}]
  (utils.hex/pad (if value "0x1" "0x0")))

(defmethod param :address
  [{value :value}]
  (utils.hex/pad (str/lower-case value)))

(defmethod param :bytes
  [{value :value}]
  (utils.hex/pad value {:dir :right}))

(defmethod param :unumber
  [{value :value}]
  (utils.hex/pad (format "%x" (BigInteger. (str value)))))

(defmethod param :number
  [{value :value}]
  (utils.hex/pad (format "%x" (if (< value 0)
                                (.add (BigInteger. (str value)) (.shiftLeft (BigInteger. (str "1")) (* 32 8)))
                                (BigInteger. (str value))))))

(defmethod param :dbytes
  [{value :value}]
  (let [size (utils.hex/size value)
        padded (if (not= 0 (mod size 32))
                 (utils.hex/pad value {:dir :right :size (* 32 (Math/ceil (/ size 32)))})
                 value)]
    (utils.hex/concat [(param {:type "uint256" :value size}) padded])))

(defmethod param :string
  [{value :value}]
  (let [hex-value (Hex/toHexString (.getBytes value StandardCharsets/UTF_8))
        size (utils.hex/size hex-value)
        parts-length (int (Math/ceil (/ size 32)))]
    (->> (range 0 parts-length)
         (map (fn [i] (let [p (subs hex-value (* i 64) (min (count hex-value) (* (inc i) 64)))]
                        (if (= i (dec parts-length))
                          (utils.hex/pad p {:dir :right})
                          p))))
         (utils.hex/concat [(param {:type "uint256" :value size})]))))

(defmethod param :default [_] (throw (ex-info "Not implemented" {:causes :lazyness})))

(defmulti signature :type)

(defmethod signature "function"
  [function-abi-item]
  (str "0x" (-> (utils.abi/item->signature function-abi-item)
                utils.hex/keccak-256
                (subs 0 8))))

(defmethod signature "event" [event-abi-item]
  (str "0x"
       (-> (utils.abi/item->signature event-abi-item)
           utils.hex/keccak-256)))

(defmethod signature :default [_] (throw (ex-info "Not implemented" {:causes :lazyness})))

(defn function-call
  [{abi-item :abi-item args :args}]
  (utils.hex/concat [(signature abi-item)
                     (param {:type "tuple" :components (:inputs abi-item) :value args})]))
