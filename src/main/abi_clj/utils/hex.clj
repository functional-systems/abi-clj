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

(defn pad
  ([value] (pad value {:dir :left}))
  ([value {:keys [dir]}]
   (let [s (str/replace value #"^0x" "")
         pad (str/join (take (- 64 (count s)) (repeat "0")))]
     (apply str "0x" (if (= dir :left) [pad s] [s pad])))))

(defn concat [values]
  (->> values
       (map #(str/replace % #"^0x" ""))
       (clojure.core/concat ["0x"])
       (str/join)))

(defn size [value]
  (-> value
      (str/replace #"^0x" "")
      count
      (/ 2)))
