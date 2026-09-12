(ns autocode.tui
  "Minimal terminal view. Zero third-party deps.
   Mirrors autocode/tui.py behaviour in a Clojure-idiomatic way:
   streaming markdown-ish output, gray thinking, tool previews.
   Non-TTY output is left untouched."
  (:require [clojure.string :as str])
  (:import [java.io BufferedReader InputStreamReader]))

(def ^:private ansi?
  (delay
    (boolean
     (and (some? (System/console))
          (not= "dumb" (System/getenv "TERM"))
          (some? (System/getenv "TERM"))))))

(def ^:private codes
  {:reset "\u001B[0m" :bold "\u001B[1m" :italic "\u001B[3m"
   :gray "\u001B[90m" :red "\u001B[31m" :yellow "\u001B[33m"
   :magenta "\u001B[35m" :cyan "\u001B[36m"})

(defn- color
  [s c]
  (if @ansi?
    (str (get codes c "") s (:reset codes))
    s))

(def ^:private latex-map
  {"\\alpha" "α" "\\beta" "β" "\\gamma" "γ" "\\delta" "δ"
   "\\pi" "π" "\\sigma" "σ" "\\lambda" "λ" "\\mu" "μ"
   "\\times" "×" "\\cdot" "·" "\\leq" "≤" "\\geq" "≥"
   "\\neq" "≠" "\\infty" "∞" "\\sqrt" "√" "\\sum" "∑"})

(defn latex->unicode
  "Tiny subset: \\frac{1}{2} -> ½, plus greek map. Good enough for stream preview."
  [s]
  (let [s (str/replace
           s #"\\frac\{1\}\{2\}" "½")]
    (reduce (fn [acc [k v]] (str/replace acc k v))
            s latex-map)))

(defonce ^:private cur-kind (atom nil))

(defn tty? [] (some? (System/console)))

(defn banner
  [model session cwd]
  (binding [*out* *err*]
    (println (color (str "autocode · " model " · session " session " · /help for commands")
                    :gray))
    (println (color (str "cwd " cwd) :gray))))

(defn- print-stream
  [kind text]
  (let [out (if (= kind "text") *out* *err*)
        rendered (if (= kind "reasoning")
                   (color text :gray)
                   (latex->unicode text))]
    (.write out rendered)
    (.flush out)))

(defn stream!
  "Append a streamed chunk. kind is \"text\" or \"reasoning\"."
  [kind text]
  (when (not= kind @cur-kind)
    (when @cur-kind (println) (flush))
    (reset! cur-kind kind))
  ;; Markdown headings/lists pass through; code highlight is intentionally
  ;; minimal here to stay zero-dep (full highlight lives in tui.py upstream).
  (print-stream kind text))

(defn end!
  []
  (when @cur-kind
    (if (= @cur-kind "text")
      (do (newline) (flush))
      (binding [*out* *err*] (newline) (flush)))
    (reset! cur-kind nil)))

(defn tool!
  [nm args]
  (end!)
  (binding [*out* *err*]
    (let [preview (subs (str args) 0 (min 300 (count (str args))))]
      (println (color (str "● " nm " " preview) :cyan)))))

(defn result!
  [output]
  (binding [*out* *err*]
    (doseq [line (take 6 (str/split-lines (or output "")))]
      (println (color line :gray)))))

(defn note!
  ([text] (note! text nil))
  ([text c]
   (end!)
   (binding [*out* *err*]
     (println (if c (color text c) text)))))

(defn steered!
  [text]
  (binding [*out* *err*]
    (println (color (str "> " text) :yellow))))

(defn ask
  "Read a line with % meter. Handles trailing \\ continuation."
  [meter]
  (print (str "\n" meter "% > ") )
  (flush)
  (let [r (BufferedReader. (InputStreamReader. System/in))]
    (loop [line (.readLine r)]
      (when (nil? line) (throw (ex-info "EOF" {:eof true})))
      (if (str/ends-with? line "\\")
        (do (print "… ") (flush)
            (let [nxt (.readLine r)]
              (when (nil? nxt) (throw (ex-info "EOF" {:eof true})))
              (recur (str (subs line 0 (dec (count line))) "\n" nxt))))
        line))))

(defn idle! [] (end!))

(defmacro with-working
  "Clojure port of view.working(deliver). v1: no raw-mode keystroke capture
   (JVM raw TTY needs JLine/JNI). Body runs directly; queued steering is
   drained between steps via inject!. Keeps the same call shape so a future
   JLine-backed view can drop in."
  [_queue-append & body]
  `(do ~@body))
