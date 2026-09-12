(ns autocode
  "CLI entry. Mirrors runner.py main()/repl()/slash().
   Local-only: no telemetry, no auto-update; --setup/--diff/--reset work offline."
  (:require [autocode.config :as cfg]
            [autocode.tools :as tools]
            [autocode.wire :as wire]
            [autocode.agent :as agent]
            [autocode.tui :as tui]
            [autocode.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.net URI]
           [java.time Duration])
  (:gen-class))

(def help-text
  "/help                  show this list
/model [name|filter]   list the server's models, or switch (asks whether to keep it)
/config [key [value]]  show settings and where they come from, or save one for this project
/config unset <key>    remove a project setting
/reset                 restore runner.prev.clj backup (if any) and continue
/compact               summarize the conversation now
/new                   start a new session
/exit                  quit (or Ctrl-D) · end a line with \\ to continue it
While it works, just type: your message is queued and delivered before its next step (v1: between steps).")

(defonce listed (atom []))

(declare persist-model-choice!)

;; ---------- models ----------

(defn list-models
  [config]
  (let [client (-> (HttpClient/newBuilder) (.connectTimeout (Duration/ofSeconds 15)) .build)
        url (str (str/replace (get config "base_url") #"/$" "") "/models")
        b (-> (HttpRequest/newBuilder (URI/create url)) (.timeout (Duration/ofSeconds 15)) (.GET))]
    (when (seq (get config "api_key")) (.header b "Authorization" (str "Bearer " (get config "api_key"))))
    (doseq [[k v] (get config "headers")] (.header b k (str v)))
    (let [resp (.send client (.build b) (HttpResponse$BodyHandlers/ofString))
          body (json/read-str (.body resp))]
      (->> (get body "data") (filter map?) (keep #(get % "id")) vec))))

(defn show-models [models current header]
  (reset! listed (vec (take 30 models)))
  (let [rows (cond-> []
               (seq header) (conj header)
               true (into (map-indexed (fn [i m]
                                         (str (if (= m current) "●" " ") (format "%3d  %s" (inc i) m)))
                                       @listed))
               (> (count models) (count @listed))
               (conj (str "  … " (- (count models) (count @listed)) " more; /model <text> filters")))]
    (tui/note! (str (str/join "\n" rows) "\n/model <number or name> switches"))))

(defn switch-model!
  [cfg-atom arg]
  (let [config @cfg-atom]
    (if (and (seq arg) (re-matches #"\d+" arg)
             (<= 1 (Long/parseLong arg) (count @listed)))
      (let [choice (nth @listed (dec (Long/parseLong arg)))]
        (persist-model-choice! cfg-atom choice))
      (let [models (try (list-models config)
                        (catch Exception e
                          (if (seq arg) [arg]
                              (do (tui/note! (str "model: " (get config "model")
                                                  " · couldn't list the server's models (" (.getMessage e) ")")
                                             :yellow)
                                  nil))))]
        (when models
          (cond
            (empty? arg) (show-models models (get config "model") (str "model: " (get config "model")))
            :else
            (let [matches (filter #(str/includes? (str/lower-case %) (str/lower-case arg)) models)]
              (cond
                (some #(= % arg) models) (persist-model-choice! cfg-atom arg)
                (= 1 (count matches)) (persist-model-choice! cfg-atom (first matches))
                (seq matches) (show-models matches (get config "model") "")
                :else
                (do (print (str arg " isn't listed by the server; use it anyway? [y/N] ")) (flush)
                    (let [ans (try (read-line) (catch Exception _ ""))]
                      (when (str/starts-with? (str/lower-case (str/trim (or ans ""))) "y")
                        (persist-model-choice! cfg-atom arg))))))))))))

(defn persist-model-choice!
  [cfg-atom choice]
  (print (str "Switch to " choice " for this [s]ession, this [p]roject, or all projects ([g]lobal)? [s] "))
  (flush)
  (let [ans (try (str/lower-case (str/trim (or (read-line) ""))) (catch Exception _ ""))
        w (when (seq ans) (subs ans 0 1))]
    (cond
      (= w "p") (do (cfg/save-setting! (cfg/project-config-file) "model" choice)
                    (swap! cfg/overrides dissoc "model"))
      (= w "g") (do (cfg/save-setting! (cfg/global-config-file) "model" choice)
                    (swap! cfg/overrides dissoc "model"))
      :else (swap! cfg/overrides assoc "model" choice))
    (cfg/refresh! cfg-atom)
    (when (not= (get @cfg-atom "model") choice)
      (swap! cfg/overrides assoc "model" choice)
      (cfg/refresh! cfg-atom))
    (let [kept (case w "p" (str "saved to " (cfg/short-path (cfg/project-config-file)))
                     "g" (str "saved to " (cfg/short-path (cfg/global-config-file)))
                     "this session")]
      (tui/note! (str "model → " choice " · " kept)))))

;; ---------- config slash ----------

(defn configure!
  [cfg-atom arg]
  (let [parts (str/split (str/trim (or arg "")) #"\s+" 2)
        unset? (and (= (first parts) "unset") (= 2 (count parts)))
        k (cond unset? (str/trim (second parts))
                (seq (first parts)) (first parts)
                :else nil)]
    (when (and k (not (contains? cfg/defaults k)))
      (tui/note! (str "unknown setting " (pr-str k) "; settings: " (str/join ", " (keys cfg/defaults))) :red)
      (throw (ex-info "done" {:done true})))
    (cond
      unset?
      (do (cfg/save-setting! (cfg/project-config-file) k nil :unset true)
          (cfg/refresh! cfg-atom)
          (let [[src v] (cfg/effective k)]
            (tui/note! (str k " removed from " (cfg/short-path (cfg/project-config-file))
                            " · now " (cfg/shown k v) " from " src))))
      (and k (= 2 (count parts)))
      (let [raw (second parts)
            v (try (if (string? (get cfg/defaults k)) raw (json/read-str raw))
                   (catch Exception _ ::bad))]
        (if (= v ::bad)
          (tui/note! (str k " takes a JSON value: a number, true/false, null, or an object") :red)
          (do (cfg/save-setting! (cfg/project-config-file) k v)
              (cfg/refresh! cfg-atom)
              (let [[src _] (cfg/effective k)
                    note (str k " = " (cfg/shown k v) " · saved to "
                              (cfg/short-path (cfg/project-config-file)))]
                (tui/note! (cond-> note
                             (not= src (cfg/short-path (cfg/project-config-file)))
                             (str " · " src " still takes precedence")
                             (and (= k "api_key") (string? v) (seq v) (not (str/starts-with? v "$")))
                             (str " · tip: save a $VAR reference instead of the key itself")))))))
      :else
      (let [ks (if k [k] (keys cfg/defaults))
            rows (map (fn [kk]
                        (let [[src v] (cfg/effective kk)]
                          (format "%-15s %-44s %s" kk (cfg/shown kk v) src)))
                      ks)]
        (tui/note! (str (str/join "\n" rows)
                        (when-not k (str "\n/config <key> <value> saves to "
                                         (cfg/short-path (cfg/project-config-file))))))))))

;; ---------- reset/diff ----------

(defn do-reset!
  [st]
  (let [prev (io/file (cfg/home) "runner.prev.clj")
        mains (agent/self-files)]
    (if (and (.exists prev) (seq mains))
      (do (spit (first mains) (slurp prev))
          (tui/note! "restored from .autocode/runner.prev.clj · reloading" :magenta)
          (agent/reload-if-changed! st))
      (tui/note! "no .autocode/runner.prev.clj backup to restore" :yellow))))

(defn do-diff!
  []
  (let [prev (io/file (cfg/home) "runner.prev.clj")
        mains (agent/self-files)]
    (if (and (.exists prev) (seq mains))
      (let [r (tools/bash {:command (str "diff -u " (pr-str (str prev)) " " (pr-str (str (first mains)))
                                          " | head -200")})]
        (tui/note! (if (seq r) r "(no diff)")))
      (tui/note! "no backup to diff against yet (edit a source file first)" :yellow))))

;; ---------- slash ----------

(defn slash!
  [st cfg-atom line]
  (let [t (str/trim line)
        i (str/index-of t " ")
        nm (if i (subs t 0 i) t)
        arg (if i (str/trim (subs t (inc i))) "")]
    (case nm
      "/help" (tui/note! help-text)
      "/new" (let [n (agent/make-agent cfg-atom)]
               (tui/note! (str "new session " (:sid @n)))
               n)
      "/compact" (agent/compact! st)
      "/model" (switch-model! cfg-atom (or arg ""))
      "/config" (try (configure! cfg-atom (or arg ""))
                     (catch clojure.lang.ExceptionInfo e
                       (when-not (:done (ex-data e)) (throw e))))
      "/reset" (do-reset! st)
      "/diff" (do-diff!)
      nil)))

;; ---------- repl ----------

(defn repl!
  [st cfg-atom]
  (tui/banner (get @cfg-atom "model") (:sid @st) (str (cfg/root)))
  (loop [st st]
    (let [pct (int (/ (* 100 (agent/context-est st))
                      (max 1 (long (or (get @cfg-atom "context_window") 128000)))))
          line (try (tui/ask (str pct))
                    (catch clojure.lang.ExceptionInfo e
                      (if (:eof (ex-data e)) ::eof (throw e))))]
      (cond
        (= line ::eof) (do (tui/note! (str "session saved · resume with: clojure -M -m autocode -r " (:sid @st)))
                           st)
        (not (seq (str/trim (or line "")))) (recur st)
        (contains? #{"/exit" "/quit"} (str/trim line)) (do (tui/note! (str "session saved · resume with: clojure -M -m autocode -r "
                                                                           (:sid @st)))
                                                          st)
        :else
        (let [is-cmd? (str/starts-with? (str/trim line) "/")
              res (try
                    (if is-cmd?
                      (let [r (slash! st cfg-atom line)]
                        (if (and r (not= r st)) {:new-st r} {:same true}))
                      (do (agent/turn! st line)
                          ;; drain messages typed right as turn ended
                          (loop []
                            (when (seq (:queue @st))
                              (agent/turn! st)
                              (recur)))
                          {:same true}))
                    (catch clojure.lang.ExceptionInfo e
                      (tui/note! (str "[" (first (str/split (str/trim line) #"\s+")) " stopped] "
                                      (.getMessage e)) :red)
                      {:same true})
                    (catch Exception e
                      (tui/note! (str "error: " (.getMessage e)) :red)
                      {:same true}))]
          (recur (or (:new-st res) st)))))))

;; ---------- setup (local) ----------

(defn run-setup!
  [cfg-atom]
  (println "autocode setup (local only). Picks base_url + model, tests one call, saves to ~/.config/autocode/config.json")
  (print (str "base_url [" (get @cfg-atom "base_url") "]: "))
  (flush)
  (let [b (str/trim (or (read-line) ""))]
    (when (seq b)
      (cfg/save-setting! (cfg/global-config-file) "base_url" b)))
  (cfg/refresh! cfg-atom)
  (let [models (try (list-models @cfg-atom) (catch Exception _ []))]
    (if (seq models)
      (do
        (show-models models (get @cfg-atom "model") "")
        (print "model (number or name): ")
        (flush)
        (let [ans (str/trim (or (read-line) ""))]
          (when (seq ans)
            (let [choice (if (re-matches #"\d+" ans)
                           (get models (dec (Long/parseLong ans)))
                           ans)]
              (when choice
                (cfg/save-setting! (cfg/global-config-file) "model" choice))))))
      (do
        (print (str "model [" (get @cfg-atom "model") "]: "))
        (flush)
        (let [ans (str/trim (or (read-line) ""))]
          (when (seq ans)
            (cfg/save-setting! (cfg/global-config-file) "model" ans))))))
  (cfg/refresh! cfg-atom)
  (println (str "saved. model=" (get @cfg-atom "model") " base_url=" (get @cfg-atom "base_url")))
  (println "Tip: store keys as $VAR refs, e.g. {\"api_key\":\"$OPENAI_API_KEY\"}. No network used except the test call above."))

;; ---------- args ----------

(defn parse-args
  [args]
  (loop [xs (vec args) acc {:prompt []}]
    (if (empty? xs)
      acc
      (let [a (first xs) r (subvec xs 1)]
        (cond
          (contains? #{"-p" "--print"} a) (recur r (assoc acc :print true))
          (contains? #{"-c" "--continue"} a) (recur r (assoc acc :cont true))
          (contains? #{"-r" "--resume"} a) (recur (subvec xs 2) (assoc acc :resume (second xs)))
          (contains? #{"-m" "--model"} a) (recur (subvec xs 2) (assoc acc :model (second xs)))
          (= a "--continue-turn") (recur r (assoc acc :continue-turn true))
          (= a "--reset") (recur r (assoc acc :reset true))
          (= a "--diff") (recur r (assoc acc :diff true))
          (= a "--setup") (recur r (assoc acc :setup true))
          (contains? #{"-h" "--help"} a) (recur r (assoc acc :help true))
          (str/starts-with? a "-") (recur r (assoc acc :unknown a))
          :else (recur r (update acc :prompt conj a)))))))

(defn read-piped-stdin
  []
  (try
    (when (pos? (.available System/in))
      (let [b (.readAllBytes System/in)]
        (when (pos? (count b)) (String. b "UTF-8"))))
    (catch Exception _ nil)))

(defn -main
  [& args]
  (let [par (parse-args args)]
    (when (:help par)
      (println "autocode (Clojure) — minimal self-mutating coding agent, local only")
      (println "usage: clojure -M -m autocode [prompt] [-p] [-c] [-r ID] [-m MODEL] [--setup] [--diff] [--reset]")
      (System/exit 0))
    (when (:unknown par)
      (binding [*out* *err*] (println (str "unknown flag " (:unknown par))))
      (System/exit 2))
    (when (:model par) (swap! cfg/overrides assoc "model" (:model par)))
    (let [cfg-atom (atom (cfg/load-config))]
      (when (:setup par) (run-setup! cfg-atom) (System/exit 0))
      (when (empty? (get @cfg-atom "model"))
        (binding [*out* *err*]
          (println "No model configured. Run `clojure -M -m autocode --setup`, or set AUTOCODE_MODEL, or \"model\" in .autocode/config.json or ~/.config/autocode/config.json."))
        (System/exit 2))
      (.mkdirs (io/file (cfg/home) "sessions"))
      (.mkdirs (io/file (cfg/home) "tools"))
      (let [gi (io/file (cfg/home) ".gitignore")]
        (when-not (.exists gi)
          (spit gi "config.json\nsessions/\nout/\nhistory\nrunner.prev.clj\n")))
      (agent/snapshot-sources!)
      (when (:diff par) (do-diff!) (System/exit 0))
      (let [sid (:resume par)
            sid (if (:cont par)
                  (let [dir (io/file (cfg/home) "sessions")
                        fs (when (.isDirectory dir)
                             (filter #(.isFile %) (.listFiles dir)))]
                    (if (seq fs)
                      (str/replace (.getName (last (sort-by #(.lastModified %) fs))) #"\.edn$" "")
                      nil))
                  sid)]
        (when (and sid (not (.exists (agent/session-file sid))))
          (binding [*out* *err*] (println (str "No session " (pr-str sid) " in " (io/file (cfg/home) "sessions"))))
          (System/exit 2))
        (let [piped (read-piped-stdin)
              prompt (str/join " " (:prompt par))
              prompt (str/trim (str prompt (when piped (str "\n\n" piped))))]
          (when (:reset par)
            (let [st (agent/make-agent cfg-atom :sid sid)]
              (do-reset! st)
              (System/exit 0)))
          (let [st (agent/make-agent cfg-atom :sid sid
                                     :flags (if (:print par) ["--print"] []))
                ok (atom true)]
            (when (and (:continue-turn par) (seq (:messages @st))
                       (contains? #{"user" "tool"} (get (last (:messages @st)) "role")))
              (reset! ok (agent/turn! st)))
            (when (seq prompt)
              (reset! ok (agent/turn! st prompt)))
            (when (:print par) (System/exit (if @ok 0 2)))
            (repl! st cfg-atom)))))))
