(ns abi-clj.encode
  (:require
   [abi-clj.utils.abi :as utils.abi]
   [abi-clj.utils.hex :as utils.hex]
   [clojure.string :as str])
  (:import
   [java.math BigInteger]
   [java.nio.charset StandardCharsets]
   [org.bouncycastle.util.encoders Hex]))

(defn- encode-dispatch [{type :type}]
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

(defmulti param #'encode-dispatch)

(defmethod param :tuple
  [{value :value
    components :components}]
  (if (map? value)
    (->> components
         (map (fn [type] (param (merge type {:value (or (get value (keyword (:name type)))
                                                        (get value (:name type)))}))))
         utils.hex/concat)
    (->> (zipmap components value)
         (map (fn [[type value]] (param (merge type {:value value}))))
         utils.hex/concat)))

(defmethod param :vector
  [{value :value
    type  :type}]
  (let [match (re-matches #"^((uint|int|bool|address|bytes)(\d+)?)((\[\d+\])+)$" type)
        type (second match)
        dimension (nth match 4)
        rem-dim (re-matches #"((\[\d+\])+)(\[\d+\])$" dimension)
        conc-dim (when rem-dim (second rem-dim))]
    (->> value
         (map #(param {:type (str type conc-dim) :value %}))
         utils.hex/concat)))

(defmethod param :bool
  [{value :value}]
  (utils.hex/pad (if value "0x1" "0x0")))

(defmethod param :address
  [{value :value}]
  (utils.hex/pad (str/lower-case value)))

(defmethod param :bytes
  [{value :value}]
  (utils.hex/pad value {:dir :right}))

(defmethod param :dbytes
  [{value :value}]
  (let [size (utils.hex/size value)
        padded (if (not= 0 (mod size 32))
                 (utils.hex/pad value {:dir :right :size (* 32 (Math/ceil (/ size 32)))})
                 value)]
    (utils.hex/concat [(param {:type "uint" :value size}) padded])))

(defmethod param :unumber
  [{value :value}]
  (utils.hex/pad (format "%x" (BigInteger. (str value)))))

(defmethod param :number
  [{value :value}]
  (utils.hex/pad (format "%x" (if (< value 0)
                                (.add (BigInteger. (str value)) (.shiftLeft (BigInteger. (str "1")) (* 32 8)))
                                (BigInteger. (str value))))))

(defmethod param :string
  [{value :value}]
  (let [hex-value (str "0x" (Hex/toHexString (.getBytes value StandardCharsets/UTF_8)))
        size (utils.hex/size hex-value)
        parts-length (Math/ceil (/ size 32))]
    (->> (range 0 parts-length)
         (map (fn [i] (utils.hex/pad (subs hex-value (* i 32) (min (count hex-value) (* (inc i) 32))) {:dir :right})))
         (concat [(param {:type "uint" :value size})])
         utils.hex/concat)))

(defn function-signature [function-abi-item]
  (str "0x" (-> (utils.abi/function-item->signature function-abi-item)
                utils.hex/keccak-256
                (subs 0 8))))

(defn event-signature [event-abi-item]
  (str "0x"
       (-> (utils.abi/function-item->signature event-abi-item)
           utils.hex/keccak-256)))

(defn function-data
  [{abi-item :abi-item args :args}]
  (utils.hex/concat [(function-signature abi-item)
                     (param {:type "tuple" :components (:inputs abi-item) :value args})]))
