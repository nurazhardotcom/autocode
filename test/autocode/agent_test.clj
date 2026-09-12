(ns autocode.agent-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [autocode.agent :as agent]
            [autocode.config :as cfg]
            [clojure.java.io :as io]))

(defn- with-tmp-root [f]
  (let [orig (System/getProperty "user.dir")
        tmp (str (java.nio.file.Files/createTempDirectory
                  "autocode-agent-test" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (System/setProperty "user.dir" tmp)
    (try (f) (finally (System/setProperty "user.dir" orig)))))

(use-fixtures :each with-tmp-root)

(deftest add-merges-user
  (testing "consecutive user messages merge to keep roles alternating"
    (let [cfg-atom (atom {"context_window" 1000 "compact_at" 0.8 "output_limit" 100
                          "max_tokens" nil "keep_reasoning" false})
          st (agent/make-agent cfg-atom :sid "t1")]
      (agent/add! st {"role" "user" "content" "a"})
      (agent/add! st {"role" "user" "content" "b"})
      (is (= 1 (count (:messages @st))))
      (is (clojure.string/includes? (get (first (:messages @st)) "content") "a")))))

(deftest limit-leaves-room
  (testing "compact limit respects max_tokens"
    (let [cfg-atom (atom {"context_window" 1000 "compact_at" 0.8 "output_limit" 100
                          "max_tokens" 200 "keep_reasoning" false})
          st (agent/make-agent cfg-atom :sid "t2")]
      (is (= 800 (agent/limit st))))))

(deftest fit-tail-clips
  (testing "oversize tool tail is clipped, not dropped, when rest fits"
    (let [cfg-atom (atom {"context_window" 2000 "compact_at" 0.8 "output_limit" 100
                          "max_tokens" nil "keep_reasoning" false})
          st (agent/make-agent cfg-atom :sid "t3")
          tail [{"role" "assistant" "content" "x"}
                {"role" "tool" "content" (apply str (repeat 3000 "y"))}]
          fit (agent/fit-tail st tail)]
      (is (seq fit))
      (is (< (count (str fit)) 4000)))))

(deftest system-includes-agents-md
  (testing "AGENTS.md augments the 3-line prompt"
    (spit (io/file (cfg/root) "AGENTS.md") "# rules\nBe brief.")
    (let [s (agent/system-msg)]
      (is (clojure.string/includes? (get s "content") "Be brief.")))))
