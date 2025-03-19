(ns playground
  (:require
   [abi-clj.decode :as decode]
   [abi-clj.encode :as encode]
   [abi-clj.utils :as utils.abi]
   [clj-http.client :as http]
   [clojure.data.json :as json]
   [clojure.string :as str]
   [abi-clj.utils.hex :as utils.hex]))

(def abi (json/read-str (slurp "./resources/abi/V3Vault.json")
                        :key-fn keyword))

(def loanInfo (first (filter (comp #{"loanInfo"} :name) abi)))

(encode/signature loanInfo)

(utils.abi/item->human-readable loanInfo)

(utils.abi/item->signature loanInfo)

(def loan-info-call (encode/function-call {:abi-item loanInfo
                                           :args [3794917]}))

(def network->rpc
  {:arbitrum (System/getenv "ARBITRUM_RPC_URL")
   :optimism (System/getenv "OPTIMISM_RPC_URL")
   :mainnet  (System/getenv "MAINNET_RPC_URL")
   :base     (System/getenv "BASE_RPC_URL")})

(defn http-rpc-call! [network rpc]
  (let [url (network->rpc network)
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

(def res (http-rpc-call! :arbitrum
                         {:jsonrpc "2.0"
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

(def swap-event-abi (->> (json/read-str (slurp "./resources/abi/UniV3Pool.json")
                                        :key-fn keyword)
                         (filter (comp #{"Swap"} :name))
                         first))

(def swaps-topic (encode/signature swap-event-abi))

(comment

  (def curr-bn (BigInteger. (str/replace (:result (http-rpc-call! :arbitrum
                                                                  {:jsonrpc "2.0"
                                                                   :method "eth_blockNumber"
                                                                   :id 10}))
                                         #"0x" "")
                            16))

  (def res (http-rpc-call! :arbitrum
                           {:jsonrpc "2.0"
                            :method "eth_getLogs"
                            :id 10
                            :params [{:fromBlock (utils.hex/number->hex (- curr-bn 10))
                                      :topics [swaps-topic]}]}))

  (decode/event {:abi-item swap-event-abi :event (first (:result res))})

;;
  )

(def all-sugar-abi (->> (json/read-str (slurp "./resources/abi/VeloSugar.json")
                                       :key-fn keyword)
                        (filter (comp #{"all"} :name))
                        first))

(def all-sugar-call (encode/function-call {:abi-item all-sugar-abi
                                           :args [1 0]}))

(comment

  (def res (http-rpc-call! :optimism
                           {:jsonrpc "2.0"
                            :method "eth_call"
                            :id 10
                            :params [{:to "0x35F233BE126d7D08aB2D65E647E8c379b1FACF39"
                                      :data all-sugar-call}]}))

  (decode/function-result {:abi-item all-sugar-abi :data (:result res)})

  (decode/param (assoc (first (:outputs all-sugar-abi)) :data (:result res)))

  (count (:result res))

;;
  )

(def slot0-abi (->> (json/read-str (slurp "./resources/abi/UniV3Pool.json")
                                   :key-fn keyword)
                    (filter (comp #{"slot0"} :name))
                    first))

(def slot0-call (encode/function-call {:abi-item slot0-abi}))

(comment

  (def res (http-rpc-call! :arbitrum
                           {:jsonrpc "2.0"
                            :method "eth_call"
                            :id 10
                            :params [{:to "0x2f5e87c9312fa29aed5c179e456625d79015299c"
                                      :data slot0-call}
                                     "latest"]}))

  (decode/function-result {:abi-item slot0-abi :data (:result res)})

  ;;
  )
