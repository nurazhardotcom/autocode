(ns autocode.agent
  "Agent loop. Clojure-native reimagining of runner.py Agent:
   immutable message maps, atom state, EDN sessions, file-tool reload,
   hot self-reload via process relaunch."
  (:require [autocode.config :as cfg]
            [autocode.tools :as tools]
            [autocode.wire :as wire]
            [autocode.tui :as tui]
            [autocode.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.time LocalDate]
           [java.time.format DateTimeFormatter]))

(def system-template
  "You are autocode, a coding agent working in {root} ({os}, {date}), with a person at the keyboard. Work until the task is done, verifying as you go, then reply briefly. If the request is ambiguous or a decision is theirs to make, end your turn and ask instead of assuming.\nYour implementation is {self}, Clojure files you may improve; edits take effect after each step. Your state (config, sessions, tools) lives in {home}.\nTo add a tool, write {home}/tools/<name>.clj defining SCHEMA (an OpenAI function schema map) and (run args-map) -> str; it is available from the next step.")

(def compact-instructions
  "Your context is full, so this conversation is about to be replaced by your summary of it. Write the summary you need to continue seamlessly: the user's requests and constraints (quote the precise ones), what has been done and learned, files changed, the current state, and the exact next steps. Be dense and complete.")

;; ---------- self sources ----------

(defn self-files
  "All agent source files to watch for self-modification."
  []
  (let [r (cfg/root)
        dir (io/file r "src" "autocode")]
    (if (.isDirectory dir)
      (->> (.listFiles dir)
           (filter #(str/ends-with? (.getName %) ".clj"))
           (sort-by #(.getName %)))
      (let [cp (System/getProperty "java.class.path" "")
            cand (io/file r "autocode.clj")]
        (if (.exists cand) [cand] [])))))

(defonce start-sources (atom nil))
(defonce seen-broken (atom #{}))

(defn snapshot-sources!
  []
  (reset! start-sources
          (into {} (map (fn [^java.io.File f]
                          [(str f) (try (slurp f) (catch Exception _ ""))])
                        (self-files))))
  (reset! seen-broken #{}))

(defn- read-all-forms?
  "True if source parses as Clojure forms (syntax check, no eval)."
  [s]
  (try
    (with-in-str s
      (loop []
        (let [f (read {:eof ::eof :read-cond :allow} *in*)]
          (when (not= f ::eof) (recur)))))
    true
    (catch Throwable _ false)))

;; ---------- agent state ----------

(defn sid-now []
  (let [fmt (DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss")]
    (.format (java.time.LocalDateTime/now) fmt)))

(defn session-file [sid]
  (io/file (cfg/home) "sessions" (str sid ".edn")))

(defn make-agent
  [cfg-atom & {:keys [sid flags] :or {flags []}}]
  (let [sid (or sid (sid-now))
        f (session-file sid)
        msgs (if (.exists f)
               (try
                 (into [] (comp (filter (complement str/blank?))
                                (map read-string))
                       (str/split-lines (slurp f)))
                 (catch Exception _ []))
               [])]
    (atom {:cfg cfg-atom :flags flags :sid sid :file f
           :messages msgs :tokens 0 :counted 0 :floor 0
           :queue []})))

(defn- persist! [st]
  (let [{:keys [file messages]} @st]
    (.mkdirs (.getParentFile ^java.io.File file))
    (spit file (str/join "\n" (map pr-str messages)))))

(defn add!
  [st msg]
  (swap! st
         (fn [s]
           (let [msgs (:messages s)
                 last-m (last msgs)]
             (if (and last-m (= (get last-m "role") (get msg "role")) (= (get msg "role") "user"))
               (assoc s :messages
                      (conj (pop msgs)
                            (update last-m "content" str "\n\n" (get msg "content"))))
               (update s :messages conj msg)))))
  (persist! st))

(defn context-est
  [st]
  (let [{:keys [tokens counted messages]} @st]
    (+ tokens
       (quot (reduce + 0 (map #(count (pr-str %)) (drop counted messages))) 4))))

(defn system-msg []
  (let [txt (-> system-template
                (str/replace "{root}" (str (cfg/root)))
                (str/replace "{os}" (System/getProperty "os.name"))
                (str/replace "{date}" (str (LocalDate/now)))
                (str/replace "{self}" (str/join ", " (map str (self-files))))
                (str/replace "{home}" (str (cfg/home))))]
    (let [extra (some (fn [nm]
                        (let [f (io/file (cfg/root) nm)]
                          (when (.exists f) [nm (slurp f)])))
                      ["AGENTS.md" "CLAUDE.md"])]
      {"role" "system"
       "content" (if extra
                   (str txt "\n\n# " (first extra) "\n\n" (second extra))
                   txt)})))

(defn limit
  [st]
  (let [cfg @(:cfg @st)
        window (long (or (get cfg "context_window") 128000))
        max-toks (get cfg "max_tokens")]
    (max (quot window 2)
         (min (long (* (double (or (get cfg "compact_at") 0.8)) window))
              (- window (long (or max-toks 0)))))))

(declare compact!)

(defn- emit-fn [_st] (fn [kind text] (tui/stream! kind text)))

(defn call!
  [st tool-schemas]
  (let [{:keys [cfg messages]} @st]
    (wire/chat @cfg (into [(system-msg)] messages) tool-schemas (emit-fn st))))

(defn run-tool!
  [st tools call]
  (let [nm (get-in call ["function" "name"])
        raw (or (get-in call ["function" "arguments"]) "{}")
        args (try (or (json/read-str raw) {})
                  (catch Exception e
                    (tui/note! (str "● " nm ": invalid arguments") :red)
                    ::bad))]
    (if (= args ::bad)
      (str "Invalid JSON arguments: " (subs raw 0 (min 500 (count raw))))
      (do
        (tui/tool! nm args)
        (if-not (contains? tools nm)
          (str "Unknown tool " (pr-str nm) ". Available: " (str/join ", " (keys tools)))
          (let [out (try (str ((get-in tools [nm :fn]) args))
                         (catch Throwable t
                           (str "Tool threw " (.getSimpleName (class t)) ": " (.getMessage t)
                                "\n" (str/join "\n" (take 20 (.getStackTrace t))))))
                cfg @(:cfg @st)
                clipped (tools/clip out (get cfg "output_limit"))]
            (tui/result! clipped)
            clipped))))))

(defn settle!
  [st]
  (let [{:keys [messages]} @st
        idx (loop [i (dec (count messages))]
              (cond (< i 0) nil
                    (= (get (nth messages i) "role") "assistant") i
                    :else (recur (dec i))))]
    (when (some? idx)
      (let [answered (into #{} (keep #(get % "tool_call_id")
                                     (subvec messages (inc idx))))
            missing (filter #(not (contains? answered (get % "id")))
                            (get (nth messages idx) "tool_calls" []))]
        (doseq [c missing]
          (add! st {"role" "tool" "tool_call_id" (get c "id")
                    "content" "[interrupted by user]"}))))))

(defn inject!
  [st]
  (loop []
    (let [q (:queue @st)]
      (when (seq q)
        (let [text (first q)]
          (swap! st update :queue subvec 1)
          (tui/steered! text)
          (add! st {"role" "user" "content" text})
          (recur))))))

(defn fit-tail
  [st tail]
  (let [cfg @(:cfg @st)
        budget (long (or (get cfg "context_window") 128000))
        size (fn [ms] (reduce + 0 (map #(count (json/write-str %)) ms)))
        results (filter #(= (get % "role") "tool") tail)
        rest (- (size tail) (size results))]
    (cond
      (<= (size tail) budget) tail
      (and (seq results) (< rest (quot budget 2)))
      (let [share (quot (* (- budget rest) 3) (* 4 (count results)))
            clipped (mapv (fn [m]
                            (if (= (get m "role") "tool")
                              (assoc m "content" (tools/clip (get m "content") share))
                              m))
                          tail)]
        (when (<= (size clipped) budget) clipped))
      :else [])))

(defn- summarize
  [st history instructions]
  (let [tools (tools/load-tools)
        schemas (tools/tool-schemas tools)
        cfg @(:cfg @st)]
    (let [attempt
          (try
            (let [[msg _] (wire/chat cfg (into [(system-msg)] (conj (vec history) {"role" "user" "content" instructions}))
                                     schemas (fn [_ _]) :extra {"tool_choice" "none"})]
              (when (and (empty? (get msg "tool_calls"))
                         (seq (str/trim (or (get msg "content") ""))))
                (str/trim (get msg "content"))))
            (catch clojure.lang.ExceptionInfo e
              (when-not (wire/context-full? e) nil))
            (catch Exception _ nil))]
      (if attempt
        attempt
        (loop [limits [2000 200]]
          (if (empty? limits)
            (throw (wire/context-full "too large to summarize, even with tool output cut short; start a /new session"))
            (let [lim (first limits)
                  req (str "<transcript>\n" (wire/transcript history lim) "\n</transcript>\n\n" instructions)]
              (or (try
                    (let [[msg _] (wire/chat cfg [(system-msg) {"role" "user" "content" req}]
                                             nil (fn [_ _]))]
                      (let [s (str/trim (or (get msg "content") ""))]
                        (when (seq s) s)))
                    (catch clojure.lang.ExceptionInfo e
                      (when-not (wire/context-full? e)
                        (throw e))
                      nil))
                  (recur (rest limits))))))))))

(defn compact!
  [st]
  (let [{:keys [messages file sid]} @st]
    (let [[history pending] (if (and (seq messages) (= (get (last messages) "role") "user"))
                              [(pop messages) [(last messages)]]
                              [messages []])]
      (when (seq history)
        (let [cut (or (last (keep-indexed (fn [i m] (when (= (get m "role") "assistant") i)) history))
                      (count history))
              recent (fit-tail st (subvec (vec history) cut))
              note (if (seq recent)
                     "\n\nYour latest step (your last message and its tool results) stays verbatim after the summary."
                     "")
              summary (summarize st history (str compact-instructions note))]
          (.mkdirs (io/file (cfg/home) "sessions" "old"))
          (when (.exists ^java.io.File file)
            (spit (io/file (cfg/home) "sessions" "old"
                           (str sid "-" (System/currentTimeMillis) ".edn"))
                  (slurp file)))
          (let [resume (if (and (seq history)
                                (= (get (last history) "role") "tool")
                                (empty? recent))
                         "\n\nContinue from where you left off." "")
                compacted (into [{"role" "user"
                                  "content" (str "[Context compacted. Your summary of the conversation so far:]\n\n"
                                                 summary resume)}]
                                recent)]
            (swap! st assoc :messages compacted :tokens 0 :counted 0)
            (persist! st)
            (doseq [m pending] (add! st m))
            (tui/note! (str "[compacted to ~" (context-est st) " tokens]"))))))))

(defn reload-if-changed!
  [st]
  (let [files (self-files)]
    (doseq [^java.io.File f files]
      (let [k (str f)
            cur (try (slurp f) (catch Exception _ nil))
            orig (get @start-sources k ::missing)]
        (when (and (some? cur) (not= cur orig) (not (contains? @seen-broken k)))
          (if (read-all-forms? cur)
            (do
              ;; backup prior version
              (try
                (let [prev (io/file (cfg/home) "runner.prev.clj")]
                  (.mkdirs (.getParentFile prev))
                  (spit prev (or orig cur)))
                (catch Exception _))
              (tui/note! (str "[" (.getName f) " changed; reloading]") :magenta)
              ;; relaunch same session, continue turn
              (let [sid (:sid @st)
                    cmd (if (some? (System/getenv "CLJ_RELOAD_CMD"))
                          [(System/getenv "CLJ_RELOAD_CMD")]
                          ["clojure" "-M" "-m" "autocode"
                           "--resume" sid "--continue-turn"])]
                (persist! st)
                (tui/idle!)
                (let [pb (ProcessBuilder. ^java.util.List cmd)]
                  (.inheritIO pb)
                  (.directory pb (cfg/root))
                  (let [p (.start pb)
                        code (.waitFor p)]
                    (System/exit code)))))
            (do
              (swap! seen-broken conj k)
              (swap! st update :messages
                     (fn [ms]
                       (if (seq ms)
                         (conj (pop ms)
                               (update (last ms) "content" str
                                       "\n[" (.getName f) " was edited but not reloaded: SyntaxError]"))
                         ms)))
              (persist! st))))))))

(defn step!
  "One model call plus its tools. Returns true while the turn continues."
  [st]
  (inject! st)
  (reload-if-changed! st)
  (when (> (context-est st) (max (limit st) (:floor @st)))
    (try
      (compact! st)
      (catch clojure.lang.ExceptionInfo e
        (if (wire/context-full? e)
          (throw e)
          (tui/note! (str "[compaction failed, continuing without it: " (.getMessage e) "]") :yellow)))
      (catch Exception e
        (tui/note! (str "[compaction failed, continuing without it: " (.getMessage e) "]") :yellow)))
    (swap! st assoc :floor (+ (context-est st)
                              (quot (long (or (get @(:cfg @st) "context_window") 128000)) 10))))
  (let [tools (tools/load-tools)
        schemas (tools/tool-schemas tools)
        [msg usage] (try
                      (call! st schemas)
                      (catch clojure.lang.ExceptionInfo e
                        (if (wire/context-full? e)
                          (do (tui/end!) (compact! st)
                              (call! st schemas))
                          (throw e))))]
    (tui/end!)
    (add! st msg)
    (if usage
      (let [pt (long (or (get usage "prompt_tokens") 0))
            ct (long (or (get usage "completion_tokens") 0))
            tot (long (or (get usage "total_tokens") (+ pt ct)))
            base (if (pos? (+ pt ct)) (+ pt ct) tot)
            rt (long (or (get-in usage ["completion_tokens_details" "reasoning_tokens"]) 0))
            keep? (or (get @(:cfg @st) "keep_reasoning")
                      (contains? msg "reasoning_details"))]
        (swap! st assoc :tokens (if keep? base (- base rt)) :counted (count (:messages @st))))
      (swap! st assoc :tokens (context-est st) :counted (count (:messages @st))))
    (doseq [c (get msg "tool_calls" [])]
      (add! st {"role" "tool" "tool_call_id" (get c "id")
                "content" (run-tool! st tools c)}))
    (boolean (seq (get msg "tool_calls")))))

(defn turn!
  "Run until the model stops calling tools. Returns false on interrupt/error."
  [st & [prompt]]
  (try
    (when (some? prompt) (add! st {"role" "user" "content" prompt}))
    (tui/with-working nil
      (loop []
        (let [more? (step! st)]
          (if (or more? (seq (:queue @st)))
            (recur)
            true))))
    (catch InterruptedException _
      (tui/note! "[interrupted]" :yellow)
      (settle! st)
      false)
    (catch clojure.lang.ExceptionInfo e
      (tui/note! (str "[error] " (.getMessage e)) :red)
      (settle! st)
      false)
    (catch Exception e
      (tui/note! (str "[error] " (.getSimpleName (class e)) ": " (.getMessage e)) :red)
      (settle! st)
      false)))
