(ns autocode.json-test
  (:require [clojure.test :refer [deftest is testing]]
            [autocode.json :as json]))

(deftest roundtrip
  (is (= {"a" 1 "b" [true nil "x"]}
         (json/read-str (json/write-str {"a" 1 "b" [true nil "x"]}))))
  (is (= {"tools" [{"type" "function"}] "stream" true}
         (json/read-str (json/write-str {"tools" [{"type" "function"}] "stream" true})))))

(deftest scalars
  (testing "numbers, strings with escapes, nested"
    (is (= {"n" nil "t" true "f" false "i" 42 "d" 3.14}
           (json/read-str "{\"n\":null,\"t\":true,\"f\":false,\"i\":42,\"d\":3.14}")))
    (is (= {"s" "a\"b\nc"} (json/read-str "{\"s\":\"a\\\"b\\nc\"}")))
    (is (= [1 2 {"k" "v"}] (json/read-str "[1,2,{\"k\":\"v\"}]")))))
