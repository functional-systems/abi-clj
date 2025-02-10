(ns abi-clj.utils.hex
  (:require
   [clojure.string :as str])
  (:import
   (java.nio.charset StandardCharsets)
   (org.bouncycastle.jcajce.provider.digest Keccak$Digest256)
   (org.bouncycastle.util.encoders Hex)))

(defn keccak-256
  [input]
  (let [digest (Keccak$Digest256.)
        input-bytes (if (string? input)
                      (.getBytes input StandardCharsets/UTF_8)
                      input)]
    (.update digest input-bytes)
    (Hex/toHexString (.digest digest))))

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
