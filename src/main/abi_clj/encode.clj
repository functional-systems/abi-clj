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
    (re-matches #"^((uint|int|bool|address|bytes|tuple)(\d+)?)((\[\d*\])+)$" type) :vector
    (= type "tuple")                   :tuple
    (= type "bool")                    :bool
    (= type "address")                 :address
    (= type "string")                  :string
    (re-matches #"^uint.*" type)       :unumber
    (re-matches #"^int.*" type)        :number
    (re-matches #"^bytes(\d+)$" type)  :bytes
    (re-matches #"^bytes$" type)       :dbytes))

(defmulti param #'encode-dispatch)

(defn- wrapp-param
  ([data] (wrapp-param data 0))
  ([data len]
   {:data data
    :len len
    :size (utils.hex/size data)}))

(defn unwrap-params [params]
  (let [static-params-size (->> params
                                (filter #(= 0 (:len %)))
                                (map :size)
                                (apply +))
        dynamic-count (->> params (filter #(< 0 (:len %))) count)
        ini-offset (+ static-params-size (* 32 dynamic-count))]
    (loop [p (first params) params (rest params) offset ini-offset static [] dynamic []]
      (if p
        (if (< 0 (:len p))
          (let [pointer (:data (param {:type "uint256" :value offset}))
                len     (:data (param {:type "uint256" :value (:len p)}))]
            (recur (first params) (rest params) (+ offset (:size p)) (conj static pointer) (concat dynamic [len (:data p)])))
          (recur (first params) (rest params) offset (conj static (:data p)) dynamic))

        (utils.hex/concat static dynamic)))))

(defmethod param :tuple
  [{value :value
    components :components}]
  (wrapp-param (unwrap-params (if (map? value)
                                (->> components
                                     (map (fn [type] (param (merge type {:value (or (get value (keyword (:name type)))
                                                                                    (get value (:name type)))})))))
                                (->> (zipmap components value)
                                     (map (fn [[type value]] (param (merge type {:value value})))))))))

(defmethod param :number
  [{value :value}]
  (wrapp-param (utils.hex/pad (format "%x" (if (< value 0)
                                             (.add (BigInteger. (str value)) (.shiftLeft (BigInteger. (str "1")) (* 32 8)))
                                             (BigInteger. (str value)))))))

(defmethod param :bool
  [{value :value}]
  (wrapp-param (utils.hex/pad (if value "0x1" "0x0"))))

(defmethod param :address
  [{value :value}]
  (wrapp-param (utils.hex/pad (str/lower-case value))))

(defmethod param :bytes
  [{value :value}]
  (wrapp-param (utils.hex/pad value {:dir :right})))

(defmethod param :unumber
  [{value :value}]
  (wrapp-param (utils.hex/pad (format "%x" (BigInteger. (str value))))))

(defmethod param :vector
  [{value :value
    type  :type}]
  (let [match (re-matches #"^((uint|int|bool|address|bytes|tuple)(\d+)?)((\[\d*\])+)$" type)
        type (second match)
        dimension (nth match 4)
        rem-dim (re-matches #"((\[\d*\])+)(\[\d*\])$" dimension)
        conc-dim (when rem-dim (second rem-dim))
        dynamic? (= "[]" (last match))]
    (wrapp-param (unwrap-params (->> value
                                     (map #(param {:type (str type conc-dim) :value %}))))
                 (if dynamic? (count value) 0))))

;; (defmethod param :dbytes
;;   [{value :value}]
;;   (let [size (utils.hex/size value)
;;         padded (if (not= 0 (mod size 32))
;;                  (utils.hex/pad value {:dir :right :size (* 32 (Math/ceil (/ size 32)))})
;;                  value)]
;;     (utils.hex/concat [(param {:type "uint" :value size}) padded])))

(defmethod param :string
  [{value :value}]
  (let [hex-value (str "0x" (Hex/toHexString (.getBytes value StandardCharsets/UTF_8)))
        size (utils.hex/size hex-value)
        parts-length (Math/ceil (/ size 32))]
    (wrapp-param (->> (range 0 parts-length)
                      (map (fn [i] (utils.hex/pad (subs hex-value (* i 32) (min (count hex-value) (* (inc i) 32))) {:dir :right})))
                      utils.hex/concat)
                 (count value))))

(defmethod param :default [_] (throw (ex-info "Not implemented" {:causes :lazyness})))

(defmulti signature :type)

(defmethod signature "function"
  [function-abi-item]
  (utils.hex/concat [(-> (utils.abi/item->signature function-abi-item)
                         utils.hex/keccak-256
                         (subs 0 8))]))

(defmethod signature "event" [event-abi-item]
  (utils.hex/concat [(-> (utils.abi/item->signature event-abi-item)
                         utils.hex/keccak-256)]))

(defmethod signature :default [_] (throw (ex-info "Not implemented" {:causes :lazyness})))

(defn function-call
  [{abi-item :abi-item args :args}]
  (utils.hex/concat [(signature abi-item)
                     (unwrap-params [(param {:type "tuple" :components (:inputs abi-item) :value args})])]))
