(ns autocode.json
  "Minimal JSON reader/writer. Zero deps.
   Covers what autocode needs: string/number/boolean/null/map/vector
   for OpenAI wire payloads and config files. Not a full spec validator."
  (:require [clojure.string :as str]))

;; ---------- writing ----------

(defn- escape-str [s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")
      (str/replace "\r" "\\r")
      (str/replace "\t" "\\t")
      (str/replace "\b" "\\b")
      (str/replace "\f" "\\f")))

(declare write-val)

(defn- write-map [m sb]
  (.append sb \{)
  (let [entries (seq m)]
    (when entries
      (loop [[[k v] & rest] entries first? true]
        (when-not first? (.append sb \,))
        (.append sb \")
        (.append sb (escape-str (if (keyword? k) (name k) (str k))))
        (.append sb "\":")
        (write-val v sb)
        (when rest (recur rest false)))))
  (.append sb \}))

(defn- write-vec [v sb]
  (.append sb \[)
  (dorun (map-indexed (fn [i x]
                        (when (pos? i) (.append sb \,))
                        (write-val x sb))
                      v))
  (.append sb \]))

(defn- write-val [v ^StringBuilder sb]
  (cond
    (nil? v) (.append sb "null")
    (true? v) (.append sb "true")
    (false? v) (.append sb "false")
    (string? v) (doto sb (.append \") (.append (escape-str v)) (.append \"))
    (keyword? v) (doto sb (.append \") (.append (escape-str (name v))) (.append \"))
    (number? v) (.append sb (str v))
    (map? v) (write-map v sb)
    (sequential? v) (write-vec v sb)
    :else (doto sb (.append \") (.append (escape-str (str v))) (.append \"))))

(defn write-str
  "Serialize Clojure data to JSON string."
  [v]
  (let [sb (StringBuilder.)]
    (write-val v sb)
    (str sb)))

;; ---------- reading ----------

(defn- skip-ws [^String s ^long n]
  (loop [i n]
    (if (and (< i (.length s)) (Character/isWhitespace (.charAt s i)))
      (recur (inc i))
      i)))

(declare read-val)

(defn- read-string-lit [^String s ^long i]
  (let [sb (StringBuilder.)]
    (loop [j (inc i)]
      (let [c (.charAt s j)]
        (cond
          (= c \") [(str sb) (inc j)]
          (= c \\)
          (let [e (.charAt s (inc j))]
            (.append sb (case e
                          \" \"
                          \\ \\
                          \/ \/
                          \b \backspace
                          \f \formfeed
                          \n \newline
                          \r \return
                          \t \tab
                          ;; \uXXXX: not fully supported; keep best-effort
                          e))
            ;; \uXXXX not fully supported; pass through (wire never needs it for tests)
            (recur (+ j 2)))
          :else (do (.append sb c) (recur (inc j))))))))

(defn- read-literal [^String s ^long i]
  (cond
    (str/starts-with? (subs s i) "true") [true (+ i 4)]
    (str/starts-with? (subs s i) "false") [false (+ i 5)]
    (str/starts-with? (subs s i) "null") [nil (+ i 4)]
    :else
    (let [raw (re-find #"^-?\d+(\.\d+)?([eE][+-]?\d+)?" (subs s i))
          m (if (vector? raw) (first raw) raw)]
      (if m
        [(if (or (str/includes? m ".") (str/includes? m "e") (str/includes? m "E"))
           (Double/parseDouble m)
           (try (Long/parseLong m) (catch Exception _ (Double/parseDouble m))))
         (+ i (count m))]
        (throw (ex-info (str "bad JSON value at " i) {:pos i}))))))

(defn- read-array [^String s ^long i]
  (loop [j (skip-ws s (inc i)) acc []]
    (if (= (.charAt s j) \])
      [acc (inc j)]
      (let [[v k] (read-val s j)
            k2 (skip-ws s k)
            c (.charAt s k2)]
        (cond (= c \,) (recur (skip-ws s (inc k2)) (conj acc v))
              (= c \]) [(conj acc v) (inc k2)]
              :else (throw (ex-info "expected , or ]" {:pos k2})))))))

(defn- read-object [^String s ^long i]
  (loop [j (skip-ws s (inc i)) acc {}]
    (let [j (skip-ws s j)]
      (if (= (.charAt s j) \})
        [acc (inc j)]
        (let [[k k2] (read-string-lit s j)
              c2 (skip-ws s k2)]
          (when (not= (.charAt s c2) \:)
            (throw (ex-info "expected :" {:pos c2})))
          (let [[v k3] (read-val s (skip-ws s (inc c2)))
                k4 (skip-ws s k3)
                c (.charAt s k4)]
            (cond (= c \,) (recur (skip-ws s (inc k4)) (assoc acc k v))
                  (= c \}) [(assoc acc k v) (inc k4)]
                  :else (throw (ex-info "expected , or }" {:pos k4})))))))))

(defn- read-val [^String s ^long i]
  (let [i (skip-ws s i)
        c (.charAt s i)]
    (cond (= c \") (read-string-lit s i)
          (= c \{) (read-object s i)
          (= c \[) (read-array s i)
          :else (read-literal s i))))

(defn read-str
  "Parse a JSON string. Returns Clojure data with string keys for objects."
  [s]
  (when (and s (not (str/blank? s)))
    (first (read-val s (skip-ws s 0)))))
