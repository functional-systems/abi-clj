(ns dev.playground
  (:require
   [abi-clj.decode :as decode]
   [abi-clj.encode :as encode]
   [abi-clj.utils :as utils.abi]
   [clj-http.client :as http]
   [clojure.data.json :as json]))

(def abi (json/read-str (slurp "./resources/abi/V3Vault.json")
                        :key-fn keyword))

(def loanInfo (first (filter (comp #{"loanInfo"} :name) abi)))

(encode/function-signature loanInfo)

(utils.abi/item->human-readable loanInfo)

(utils.abi/function-item->signature loanInfo)

(def loan-info-call (encode/function-data {:abi-item loanInfo
                                           :args [3794917]}))

(defn http-rpc-call! [rpc]
  (let [url (System/getenv "ARBITRUM_RPC_URL")
        [success body]
        (try [true (-> url (http/post
                            {:accept       :json
                             :content-type :json
                             :body         (json/write-str (merge {:jsonrpc "2.0"} rpc))
                             :socket-timeout (* 2 60 1000)
                             :connection-timeout (* 2 60 1000)})
                       :body)]
             (catch Exception e [false e]))
        error (when success (get body "error"))]
    (cond
      (and success (not error)) (json/read-str body :key-fn keyword)

      error                     (throw (ex-info (format ":node-rpc-service Request error: %s"
                                                        (str error))
                                                {:causes error}))
      :else                     (throw body))))

(def res (http-rpc-call! {:jsonrpc "2.0"
                          :method "eth_call"
                          :id 10
                          :params [{:to "0x74e6afef5705beb126c6d3bf46f8fad8f3e07825"
                                    :data loan-info-call}]}))

(decode/function-result {:abi-item loanInfo
                         :data (:result res)})

(def get-pool-abi (->> (json/read-str (slurp "./resources/abi/VeloPoolFactory.json")
                                      :key-fn keyword)
                       (filter (comp #{"getPool"} :name))
                       first))

(def get-pool-human-abi (utils.abi/item->human-readable get-pool-abi))

(def mint-abi (->> (json/read-str (slurp "./resources/abi/VeloNftmanager.json")
                                  :key-fn keyword)
                   (filter (comp #{"increaseLiquidity"} :name))
                   first))

(def mint-human-abi (utils.abi/item->human-readable mint-abi))

(def swap-event-abi (->> (json/read-str (slurp "./resources/abi/UniV3Pool.abi")
                                        :key-fn keyword)
                         (filter (comp #{"Swap"} :name))
                         first))

(def swaps-topic (encode/event-signature swap-event-abi))

(comment
  (def res (http-rpc-call! {:jsonrpc "2.0"
                            :method "eth_getLogs"
                            :id 10
                            :params [{:fromBlock (format "0x%x" (BigInteger. "274782640"))
                                      :topics [swaps-topic]}]}))

  (decode/event {:abi-item swap-event-abi :event (first (:result res))})

;;
  )
