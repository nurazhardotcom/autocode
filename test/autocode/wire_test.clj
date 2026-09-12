(ns autocode.wire-test
  (:require [clojure.test :refer [deftest is testing]]
            [autocode.wire :as wire]
            [autocode.json :as json])
  (:import [com.sun.net.httpserver HttpServer HttpHandler HttpExchange]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]))

(deftest collect-text-and-tools
  (testing "streamed deltas merge like runner.py collect()"
    (let [chunks [{"choices" [{"delta" {"content" "Hel"}}]}
                  {"choices" [{"delta" {"content" "lo"}}]}
                  {"choices" [{"delta" {"tool_calls" [{"index" 0 "id" "c1"
                                                       "function" {"name" "bash" "arguments" "{\"com"}}]}}]}
                  {"choices" [{"delta" {"tool_calls" [{"index" 0
                                                       "function" {"arguments" "mand\"}"}}]}}]}
                  {"usage" {"prompt_tokens" 10 "completion_tokens" 5}}]
          emitted (atom [])
          [msg usage] (wire/collect-chunks chunks (fn [k t] (swap! emitted conj [k t])))]
      (is (= "Hello" (get msg "content")))
      (is (= 1 (count (get msg "tool_calls"))))
      (is (= "bash" (get-in msg ["tool_calls" 0 "function" "name"])))
      (is (= "{\"command\"}" (get-in msg ["tool_calls" 0 "function" "arguments"])))
      (is (= 10 (get usage "prompt_tokens"))))))

(deftest collect-reasoning
  (testing "reasoning_content + reasoning_details merge"
    (let [chunks [{"choices" [{"delta" {"reasoning_content" "think"}}]}
                  {"choices" [{"delta" {"reasoning_details" [{"index" 0 "text" " more"}]}}]}]
          [msg _] (wire/collect-chunks chunks (fn [_ _]))]
      (is (clojure.string/includes? (or (get msg "reasoning_content")
                                        (get msg "reasoning") "") "think"))
      (is (seq (get msg "reasoning_details"))))))

(deftest transcript-cuts
  (testing "tool output clipped in transcript fallback"
    (let [t (wire/transcript [{"role" "user" "content" "hi"}
                              {"role" "assistant" "content" nil
                               "tool_calls" [{"function" {"name" "bash" "arguments" (apply str (repeat 100 "a"))}}]}
                              {"role" "tool" "content" (apply str (repeat 100 "b"))}]
                             20)]
      (is (clojure.string/includes? t "[…]")))))

(deftest chat-against-fake-server
  (testing "end-to-end SSE without network (localhost only)"
    (let [server (HttpServer/create (InetSocketAddress. 0) 0)
          port (.getPort (.getAddress server))]
      (.createContext server "/chat/completions"
                      (reify HttpHandler
                        (^void handle [_ ^HttpExchange ex]
                          (let [body (str "data: " (json/write-str {"choices" [{"delta" {"content" "ok"}}]}) "\n\n"
                                          "data: [DONE]\n\n")
                                bs (.getBytes body StandardCharsets/UTF_8)]
                            (doto (.getResponseHeaders ex) (.add "Content-Type" "text/event-stream"))
                            (.sendResponseHeaders ex 200 (count bs))
                            (with-open [os (.getResponseBody ex)] (.write os bs))))))
      (.setExecutor server nil)
      (.start server)
      (try
        (let [cfg {"base_url" (str "http://127.0.0.1:" port)
                   "api_key" "" "model" "test" "context_window" 1000
                   "compact_at" 0.8 "output_limit" 1000 "timeout" 10
                   "temperature" nil "max_tokens" nil
                   "keep_reasoning" false "headers" {} "extra_body" {}}
              [msg _] (wire/chat cfg [{"role" "user" "content" "hi"}] [] (fn [_ _]))]
          (is (= "ok" (get msg "content"))))
        (finally (.stop server 0))))))
