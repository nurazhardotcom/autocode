(ns autocode.config
  "Layered configuration. Mirrors runner.py config_layers/load_config.
   Order (later wins): defaults < OPENAI_* env < global json < project json
   < AUTOCODE_* env < session overrides. $VAR refs expand from env."
  (:require [autocode.json :as json]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def defaults
  {"base_url" "https://api.openai.com/v1"
   "api_key" ""
   "model" ""
   "context_window" 128000
   "compact_at" 0.8
   "output_limit" 30000
   "timeout" 600
   "temperature" nil
   "max_tokens" nil
   "keep_reasoning" false
   "headers" {}
   "extra_body" {}})

(defonce overrides (atom {}))

(defn root [] (io/file (System/getProperty "user.dir")))
(defn home [] (io/file (root) ".autocode"))
(defn global-config-file []
  (io/file (System/getProperty "user.home") ".config" "autocode" "config.json"))
(defn project-config-file [] (io/file (home) "config.json"))

(defn short-path
  [^java.io.File f]
  (let [r (str (root)) h (System/getProperty "user.home") p (str f)]
    (-> p
        (str/replace-first (java.util.regex.Pattern/quote (str r java.io.File/separator)) "")
        (str/replace-first (java.util.regex.Pattern/quote h) "~"))))

(defn- read-json-file [f]
  (try
    (when (.exists f)
      (let [s (slurp f)]
        (if (str/blank? s) {} (or (json/read-str s) {}))))
    (catch Exception _ {})))

(defn- expand-vars [v]
  (cond
    (string? v)
    (str/replace v #"\$([A-Za-z_][A-Za-z0-9_]*)"
                 (fn [[_ nm]] (or (System/getenv nm) "")))
    (map? v) (into {} (map (fn [[k val]] [k (expand-vars val)]) v))
    (sequential? v) (mapv expand-vars v)
    :else v))

(defn config-layers
  "Vector of [source map], lowest priority first."
  []
  (let [openai-env (into {}
                         (comp (filter (fn [[_ v]] (some? v)))
                               (map (fn [[k v]] [k v])))
                         [["base_url" (System/getenv "OPENAI_BASE_URL")]
                          ["api_key" (System/getenv "OPENAI_API_KEY")]])
        auto-env (reduce (fn [acc k]
                           (if-let [raw (System/getenv (str "AUTOCODE_" (str/upper-case k)))]
                             (assoc acc k (if (string? (get defaults k))
                                            raw
                                            (try (json/read-str raw)
                                                 (catch Exception _ raw))))
                             acc))
                         {}
                         (keys defaults))]
    [["default" defaults]
     ["OPENAI_* env" openai-env]
     [(short-path (global-config-file)) (read-json-file (global-config-file))]
     [(short-path (project-config-file)) (read-json-file (project-config-file))]
     ["AUTOCODE_* env" auto-env]
     ["this session" @overrides]]))

(defn load-config
  []
  (let [merged (apply merge {} (map second (config-layers)))
        merged (update merged "api_key" expand-vars)
        merged (update merged "headers"
                       (fn [h] (into {} (map (fn [[k v]] [k (expand-vars v)]) (or h {})))))]
    merged))

(defn refresh! [cfg-atom]
  (reset! cfg-atom (load-config)))

(defn save-setting!
  [^java.io.File path k v & {:keys [unset]}]
  (let [cur (read-json-file path)
        nxt (if unset (dissoc cur k) (assoc cur k v))]
    (.mkdirs (.getParentFile path))
    (spit path (str (json/write-str nxt) "\n"))
    (try (.setReadable path false false) (catch Exception _))
    (try (.setReadable path true true) (catch Exception _))
    nxt))

(defn effective
  "Highest-priority [source value] for key."
  [k]
  (some (fn [[src m]] (when (contains? m k) [src (get m k)]))
        (reverse (config-layers))))

(defn shown
  [k v]
  (cond
    (and (= k "api_key") (string? v) (seq v) (not (str/starts-with? v "$")))
    (if (> (count v) 12) (str (subs v 0 3) "…" (subs v (- (count v) 4))) "•••")
    (= k "headers")
    (into {} (map (fn [[hk hv]]
                    [hk (if (and (string? hv) (str/starts-with? hv "$")) hv "•••")])
                  (or v {})))
    (string? v) (if (seq v) v (json/write-str v))
    :else (json/write-str v)))
