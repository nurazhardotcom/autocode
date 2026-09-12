(ns autocode.tools
  "Built-in bash tool + file-based tool loader + output clipping.
   Mirrors runner.py: persistent cwd, stdin closed, timeout, spill to file,
   SCHEMA + run convention, BROKEN marking."
  (:require [autocode.config :as cfg]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.util.concurrent TimeUnit]
           [java.nio.file Files]
           [java.nio.file.attribute FileTime]))

(def bash-schema
  {"type" "function"
   "function"
   {"name" "bash"
    "description" "Run a bash command; returns combined stdout and stderr. The working directory persists between calls. stdin is closed."
    "parameters"
    {"type" "object"
     "properties"
     {"command" {"type" "string"}
      "timeout" {"type" "integer" "description" "Seconds before the command is killed (default 120)."}}
     "required" ["command"]}}})

(defonce ^:private cwd (atom (str (cfg/root))))

(defn- base-env []
  (let [m (into {} (System/getenv))]
    (assoc m
           "PAGER" "cat"
           "GIT_PAGER" "cat"
           "GIT_EDITOR" "true"
           "GIT_TERMINAL_PROMPT" "0"
           "DEBIAN_FRONTEND" "noninteractive"
           "PYTHONUNBUFFERED" "1")))

(defn bash
  "Run command via bash -c. Returns combined stdout+stderr, tracks cwd.
   Output goes to a temp file (not a pipe) so backgrounded procs can't hang us."
  [{:keys [command timeout]}]
  (let [timeout-s (int (or timeout 120))
        tmp (Files/createTempDirectory "autocode-bash" (into-array java.nio.file.attribute.FileAttribute []))
        tmpdir (str tmp)
        out-file (io/file tmpdir "out")
        cwd-file (io/file tmpdir "cwd")
        script (str "trap 'pwd > " (pr-str (str cwd-file)) "' EXIT\n" (or command ""))]
    (try
      (let [pb (ProcessBuilder. ["bash" "-c" script])]
        (.directory pb (io/file @cwd))
        (.redirectInput pb (io/file "/dev/null"))
        (.redirectOutput pb out-file)
        (.redirectErrorStream pb true)
        (let [env (.environment pb)]
          (.clear env)
          (.putAll env (base-env)))
        (let [proc (.start pb)
              finished? (.waitFor proc timeout-s TimeUnit/SECONDS)
              code (if finished?
                     (.exitValue proc)
                     (do (-> proc .toHandle .descendants (.forEach (fn [h] (.destroyForcibly h))))
                         (.destroyForcibly proc)
                         (.waitFor proc 5 TimeUnit/SECONDS)
                         (str "killed after " timeout-s "s timeout")))]
          (try
            (when (.exists cwd-file)
              (let [nc (str/trim (slurp cwd-file))]
                (when (and (seq nc) (.isDirectory (io/file nc)))
                  (reset! cwd nc))))
            (catch Exception _))
          (let [text (try (slurp out-file) (catch Exception _ ""))]
            (if (and (number? code) (zero? code))
              (if (seq text) text "(no output)")
              (str (if (str/ends-with? text "\n") text (str text "\n"))
                   "[exit " code "]")))))
      (finally
        (try
          (doseq [f (reverse (file-seq (io/file tmpdir)))] (.delete f))
          (catch Exception _))))))

(defn clip
  "Keep text within limit chars; spill full text to .autocode/out/ and splice a pointer."
  [text limit]
  (let [text (or text "")
        limit (int (or limit 30000))]
    (if (<= (count text) limit)
      text
      (let [outdir (io/file (cfg/home) "out")]
        (.mkdirs outdir)
        (let [p (io/file outdir (str (System/nanoTime) ".txt"))]
          (spit p text)
          (let [half (quot limit 2)]
            (str (subs text 0 half)
                 "\n\n[... " (- (count text) limit) " chars omitted; full output: " (str p) " ...]\n\n"
                 (subs text (- (count text) half)))))))))

;; ---------- file-based tools ----------

(defonce ^:private loaded (atom {}))
;; key [path mtime] -> {:schema .. :fn ..}

(defn- tool-files []
  (let [d (io/file (cfg/home) "tools")]
    (when (.isDirectory d)
      (->> (.listFiles d)
           (filter #(and (.isFile %) (str/ends-with? (.getName %) ".clj")))
           (sort-by #(.getName %))))))

(defn- load-one
  [^java.io.File f]
  (let [nm (str/replace (.getName f) #"\.clj$" "")
        ns-sym (symbol (str "autocode-tool-" nm))]
    (try
      (let [ns-obj (or (find-ns ns-sym) (create-ns ns-sym))]
        (binding [*ns* ns-obj]
          (load-file (str f)))
        (let [schema-var (ns-resolve ns-obj 'SCHEMA)
              run-var (ns-resolve ns-obj 'run)]
          (if (and schema-var run-var)
            (let [raw @schema-var
                  fnm (or (get-in raw ["function" "name"])
                          (get raw "name")
                          (:name raw)
                          nm)
                  fmap (or (get raw "function") raw)
                  fmap (assoc fmap "name" fnm)
                  fmap (assoc fmap "parameters"
                              (or (get fmap "parameters")
                                  {"type" "object" "properties" {}}))]
              {:schema {"type" "function" "function" fmap}
               :fn (fn [args]
                     (let [m (into {} args)
                           kw (into {} (map (fn [[k v]] [(keyword k) v]) m))
                           both (merge m kw)]
                       (try
                         (str (@run-var both))
                         (catch clojure.lang.ArityException _
                           ;; also support (defn run [& {:keys [...]}])
                           (str (apply @run-var (mapcat identity kw)))))))})
            (throw (ex-info (str "expected SCHEMA + run in " (.getName f)) {})))))
      (catch Throwable t
        (let [err (str f " failed to load: " (.getSimpleName (class t)) ": " (.getMessage t))]
          {:schema {"type" "function"
                    "function" {"name" nm
                                "description" (str "BROKEN. " err)
                                "parameters" {"type" "object" "properties" {}}}}
           :fn (fn [_] err)})))))

(defn load-tools
  "bash plus every .autocode/tools/*.clj, reloaded when mtime changes.
   Returns {name {:schema .. :fn (args-map -> str)}}."
  []
  (let [base {"bash" {:schema bash-schema
                      :fn (fn [args]
                            (bash {:command (get args "command")
                                   :timeout (get args "timeout")}))}}
        files (or (tool-files) [])]
    (reduce (fn [acc ^java.io.File f]
              (let [k [(str f) (.lastModified f)]]
                (when-not (contains? @loaded k)
                  (swap! loaded assoc k (load-one f)))
                (let [{:keys [schema fn]} (get @loaded k)
                      nm (get-in schema ["function" "name"])]
                  (assoc acc nm {:schema schema :fn fn}))))
            base
            files)))

(defn tool-schemas
  [tools]
  (mapv :schema (vals tools)))
