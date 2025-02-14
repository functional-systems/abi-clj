(ns abi-clj.utils.hex
  (:require
   [clojure.string :as str])
  (:import
   (java.nio.charset StandardCharsets)
   (org.bouncycastle.jcajce.provider.digest Keccak$Digest256)
   (org.bouncycastle.util.encoders Hex)))

(defn ->number
  [hex]
  (BigInteger. (str/replace hex #"0x" "") 16))

(defn number->hex
  [num]
  (format "0x%x" (BigInteger. (str num))))

(defn pad
  ([value] (pad value {}))
  ([value {:keys [dir size] :or {size 32 dir :left}}]
   (let [s (str/replace value #"^0x" "")
         pad (str/join (take (- (* size 2) (count s)) (repeat "0")))]
     (apply str "0x" (if (= dir :left) [pad s] [s pad])))))

(defn concat [& values]
  (->> (reduce clojure.core/concat [] values)
       (map #(str/replace % #"^0x" ""))
       (clojure.core/concat ["0x"])
       (str/join)))

(defn size [value]
  (-> value
      (str/replace #"^0x" "")
      count
      (/ 2)))

(defn subs
  ([s start]
   (-> s
       (str/replace #"^0x" "")
       (clojure.core/subs start)
       concat))
  ([s start end]
   (-> s
       (str/replace #"^0x" "")
       (clojure.core/subs start end)
       concat)))

(defn- str->byte-arr [s]
  (let [without0x (str/replace s #"^0x" "")
        len (count without0x)
        even (= 0 (rem len 2))
        char-seq (seq without0x)
        [head body] (if even
                      [[] char-seq]
                      [[(Character/digit (first char-seq) 16)] (rest char-seq)])]
    (->>
     body
     (partition-all 2)
     (map (fn [[a b]] (+ (bit-shift-left (Character/digit a 16) 4) (Character/digit b 16))))
     (into head)
     byte-array)))

(defn- k-dispatch [s]
  (cond (and (string? s) (re-matches #"^0x.*" s)) :hex-str
        (string? s)            :str
        :else                  :bytes))

(defmulti keccak-256 k-dispatch)

(defmethod keccak-256 :hex-str
  [s]
  (keccak-256 (str->byte-arr s)))

(defmethod keccak-256 :str
  [s]
  (keccak-256 (.getBytes s StandardCharsets/UTF_8)))

(defmethod keccak-256 :bytes
  [input]
  (let [digest (Keccak$Digest256.)
        len (count input)]
    (.update digest input 0 len)
    (concat [(Hex/toHexString (.digest digest))])))
