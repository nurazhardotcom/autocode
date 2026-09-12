(ns autocode-tool-add)

(def SCHEMA
  {"name" "add"
   "description" "Add two numbers."
   "parameters" {"type" "object"
                 "properties" {"a" {"type" "number"}
                               "b" {"type" "number"}}}})

(defn run
  [{:keys [a b] :as _args}]
  (str (+ (double (or a 0)) (double (or b 0)))))
