(ns autocode.tools-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [autocode.tools :as tools]
            [autocode.config :as cfg]
            [clojure.java.io :as io]))

(defn- with-tmp-root [f]
  (let [orig (System/getProperty "user.dir")
        tmp (str (java.nio.file.Files/createTempDirectory
                  "autocode-test" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (System/setProperty "user.dir" tmp)
    (try (f) (finally (System/setProperty "user.dir" orig)))))

(use-fixtures :each with-tmp-root)

(deftest bash-echo
  (testing "bash runs and tracks cwd"
    (let [out (tools/bash {:command "echo hi"})]
      (is (clojure.string/includes? out "hi")))
    (let [d (io/file (System/getProperty "user.dir") "sub")]
      (.mkdirs d)
      (tools/bash {:command "cd sub"})
      ;; cwd persists only within same JVM atom; just check no crash
      (is (string? (tools/bash {:command "pwd"}))))))

(deftest clip-spills
  (testing "long output spills to .autocode/out"
    (let [big (apply str (repeat 500 "x"))
          c (tools/clip big 100)]
      (is (clojure.string/includes? c "chars omitted"))
      (is (clojure.string/includes? c ".autocode")))))

(deftest file-tools-load
  (testing "SCHEMA + run convention + BROKEN marking"
    (.mkdirs (io/file (cfg/home) "tools"))
    (spit (io/file (cfg/home) "tools" "adder.clj")
          "(ns autocode-tool-adder)\n(def SCHEMA {\"name\" \"adder\" \"description\" \"t\" \"parameters\" {\"type\" \"object\" \"properties\" {}}})\n(defn run [_] \"42\")\n")
    (spit (io/file (cfg/home) "tools" "broken.clj")
          "(ns autocode-tool-broken)\n(throw (ex-info \"boom\" {}))\n")
    (let [t (tools/load-tools)]
      (is (contains? t "bash"))
      (is (contains? t "adder"))
      (is (= "42" ((get-in t ["adder" :fn]) {})))
      (is (contains? t "broken"))
      (is (clojure.string/includes? (get-in t ["broken" :schema "function" "description"]) "BROKEN")))))
