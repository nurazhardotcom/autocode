(ns autocode.test-runner
  (:require [clojure.test :as t]
            autocode.json-test
            autocode.config-test
            autocode.tools-test
            autocode.wire-test
            autocode.agent-test))

(defn -main [& _]
  (let [{:keys [fail error] :as res}
        (t/run-tests 'autocode.json-test
                     'autocode.config-test
                     'autocode.tools-test
                     'autocode.wire-test
                     'autocode.agent-test)]
    (println res)
    (System/exit (if (zero? (+ fail error)) 0 1))))
