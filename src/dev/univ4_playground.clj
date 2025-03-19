(ns univ4-playground
  (:require
   [abi-clj.encode :as encode]
   [abi-clj.decode :as decode]
   [abi-clj.utils.hex :as utils.hex]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [playground]))

(defn divide [s]
  (let [size (utils.hex/size s)
        strip (str/replace s #"^0x" "")
        parts (/ size 32)]
    (->> (range 0 parts)
         (map (fn [i] (subs strip (* 64 i) (* 64 (inc i))))))))

(def abi (json/read-str (slurp "./resources/abi/UniV4PositionManager.json")
                        :key-fn keyword))

(def modify-liquidities-abi (first (filter (comp #{"modifyLiquidities"} :name) abi)))

;; Create the encoded actions for DECREASE_LIQUIDITY and TAKE_PAIR
;; Note: Actions.DECREASE_LIQUIDITY = 2, Actions.TAKE_PAIR = 7

;; Example call to collect fees for position 1150
(def collect-params (encode/param {:type "tuple"
                                   :components [{:type "uint256" :name "tokenId"}
                                                {:type "uint256" :name "liquidity"}
                                                {:type "uint128" :name "amount0Min"}
                                                {:type "uint128" :name "amount1Min"}
                                                {:type "bytes"   :name "hookData"}]
                                   :value [1150 0 0 0 "0x"]}))

(def sol-collect "0x000000000000000000000000000000000000000000000000000000000000047e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000000000000")

(= sol-collect collect-params)

(def take-params (encode/param {:type "tuple"
                                :components [{:type "address" :name "currency0"}
                                             {:type "address" :name "currency1"}
                                             {:type "address" :name "recipient"}]
                                :value ["0x0000000000000000000000000000000000000000"
                                        "0x833589fcd6edb6e08f4c7c32d4f71b54bda02913"
                                        "0x7c91d7fc5e7ff58e97960341a9b4b28d6354919d"]}))

(def sol-take "0x0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000833589fcd6edb6e08f4c7c32d4f71b54bda029130000000000000000000000007c91d7fc5e7ff58e97960341a9b4b28d6354919d")

(= sol-take take-params)

(divide take-params)

(def params (encode/param {:type "tuple"
                           :components [{:type "bytes"
                                         :name "actions"}
                                        {:type "bytes[2]"
                                         :name "params"}]
                           :value {:actions "0x0111"
                                   :params  [collect-params take-params]}}))

(utils.hex/number->hex (utils.hex/size params))

(def sol-params "0x000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000002020700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000012000000000000000000000000000000000000000000000000000000000000000c0000000000000000000000000000000000000000000000000000000000000047e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000833589fcd6edb6e08f4c7c32d4f71b54bda029130000000000000000000000007c91d7fc5e7ff58e97960341a9b4b28d6354919d")

(def collect-call (encode/function-call
                   {:abi-item modify-liquidities-abi
                    :args {:unlockData params :deadline 1739246092}}))

(def sol-collect-call "0xdd46508f00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000067aaca0c0000000000000000000000000000000000000000000000000000000000000240000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000002011100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000012000000000000000000000000000000000000000000000000000000000000000c0000000000000000000000000000000000000000000000000000000000000047e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000833589fcd6edb6e08f4c7c32d4f71b54bda029130000000000000000000000007c91d7fc5e7ff58e97960341a9b4b28d6354919d")

(= collect-call sol-collect-call)

#_(playground/http-rpc-call! :base
                             {:jsonrpc "2.0"
                              :method "eth_call"
                              :id 10
                              :params [{:to "0x7C5f5A4bBd8fD63184577525326123B519429bDc"
                                        :data (encode/function-call
                                               {:abi-item modify-liquidities-abi
                                                :args {:unlockData params :deadline (+ (quot (System/currentTimeMillis) 1000) 60)}})}]})

;;; UNIV3 Uncollected fees call works

(def nft-abi (json/read-str (slurp "./resources/abi/NFTmanager.json")
                            :key-fn keyword))

(def collect-abi (first (filter (comp #{"collect"} :name) nft-abi)))

(def uncollect-call (encode/function-call
                     {:abi-item collect-abi

                      :args {:params {:tokenId 3794917
                                      :recipient "0x859Bb79038C20F09A7Ad5726e036028f301d5de6"
                                      :amount0Max 340282366920938463463374607431768211455
                                      :amount1Max 340282366920938463463374607431768211455}}}))

(playground/http-rpc-call! :arbitrum
                           {:jsonrpc "2.0"
                            :method "eth_call"
                            :id 1
                            :params [{:to "0xc36442b4a4522e871399cd717abdd847ab11fe88"
                                      :data uncollect-call}
                                     "latest"]})

;; 1. Define the ABIs
(def position-liquidity-abi (->> (json/read-str (slurp (io/resource "abi/UniV4PositionManager.json"))
                                                :key-fn keyword)
                                 (filter (comp #{"getPositionLiquidity"} :name))
                                 first))

(def state-view-abi (->> (json/read-str (slurp (io/resource "abi/StateView.json"))
                                        :key-fn keyword)
                         (filter (comp #{"getSlot0"} :name))
                         first))

  ;; 2. Create the encoded function calls
(def position-liquidity-call (encode/function-call {:abi-item position-liquidity-abi
                                                    :args [6545]}))  ;; position ID

  ;; First get the position info
(def pool-and-position-abi (->> (json/read-str (slurp (io/resource "abi/UniV4PositionManager.json"))
                                               :key-fn keyword)
                                (filter (comp #{"getPoolAndPositionInfo"} :name))
                                first))

(def pool-and-position-call (encode/function-call {:abi-item pool-and-position-abi
                                                   :args [6545]}))  ;; Updated position ID

(def pos-res (playground/http-rpc-call! :base
                                        {:jsonrpc "2.0"
                                         :method "eth_call"
                                         :id 10
                                         :params [{:to "0x7c5f5a4bbd8fd63184577525326123b519429bdc"
                                                   :data pool-and-position-call}]}))

(def pos (decode/function-result {:abi-item pool-and-position-abi :data (:result pos-res)}))

(def pool-id "0xe87077fd043c1a6afa5256104acb1d1eb5ca5bc031ee57f9d96c8172ead4bef8")
(count pool-id)

(def slot0-call (encode/function-call {:abi-item state-view-abi
                                       :args {:poolId (utils.hex/keccak-256 (encode/param {:type "tuple"
                                                                                           :components [{:type "address"
                                                                                                         :name "currency0"}
                                                                                                        {:type "address"
                                                                                                         :name "currency1"}
                                                                                                        {:type "uint24"
                                                                                                         :name "fee"}
                                                                                                        {:type "int24"
                                                                                                         :name "tickSpacing"}
                                                                                                        {:type "address"
                                                                                                         :name "hooks"}]
                                                                                           :value (select-keys (:poolKey pos) [:currency0 :currency1 :fee :tickSpacing :hooks])}))}}))

  ;; Error here
(def slot0-res (playground/http-rpc-call! :base
                                          {:jsonrpc "2.0"
                                           :method "eth_call"
                                           :id 10
                                           :params [{:to "0xa3c0c9b65bad0b08107aa264b0f3db444b867a71" ;; StateView
                                                     :data slot0-call}]}))

(def liquidity-res (playground/http-rpc-call! :base
                                              {:jsonrpc "2.0"
                                               :method "eth_call"
                                               :id 10
                                               :params [{:to "0x7c5f5a4bbd8fd63184577525326123b519429bdc" ;; PositionManager
                                                         :data position-liquidity-call}]}))

(def position-liquidity (decode/function-result {:abi-item position-liquidity-abi
                                                 :data (:result liquidity-res)}))

(def current-pool-state (decode/function-result {:abi-item state-view-abi
                                                 :data (:result slot0-res)}))
