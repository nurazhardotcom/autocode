(ns autocode.config-test
  (:require [clojure.test :refer [deftest is testing]]
            [autocode.config :as cfg]))

(deftest shown-masks-keys
  (is (= "$OPENAI_API_KEY" (cfg/shown "api_key" "$OPENAI_API_KEY")))
  (let [s (cfg/shown "api_key" "sk-1234567890abcdef")]
    (is (clojure.string/includes? s "…")))
  (is (= {"a" "$X" "b" "•••"}
         (cfg/shown "headers" {"a" "$X" "b" "secret"}))))

(deftest defaults-present
  (testing "config layers merge without network"
    (let [layers (cfg/config-layers)]
      (is (seq layers))
      (is (= "default" (ffirst layers)))
      (is (contains? (apply merge {} (map second layers)) "model")))))
