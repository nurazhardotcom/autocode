(ns autocode.wire
  "OpenAI-compatible /chat/completions streaming client. Zero deps.
   Mirrors runner.py chat()/sse()/collect()/transcript()."
  (:require [autocode.json :as json]
            [clojure.string :as str])
  (:import [java.net.http HttpClient HttpRequest HttpResponse
            HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.net URI]
           [java.time Duration]
           [java.io BufferedReader InputStreamReader]))

(def overflow-pattern
  #"(?i)context.{0,20}(length|window|size|limit)|maximum context|context_length_exceeded|too long|too many tokens|reduce the length|exceeds? the max|token limit")

(defn api-error [msg data]
  (ex-info msg (assoc data :type ::api-error)))

(defn context-full [msg]
  (ex-info msg {:type ::context-full}))

(defn context-full? [e]
  (= ::context-full (:type (ex-data e))))

(defn- strip-reasoning
  [messages keep?]
  (if keep?
    messages
    (mapv #(dissoc % "reasoning" "reasoning_content") messages)))

(defn- clean-body
  [m]
  (into {} (filter (fn [[_ v]] (some? v)) m)))

(defn transcript
  "Conversation as plain text, tool calls/results cut to limit chars."
  [messages limit]
  (let [cut (fn [t]
              (let [t (or t "")]
                (if (<= (count t) limit) t (str (subs t 0 limit) " […]"))))]
    (str/join "\n\n"
              (mapcat
               (fn [m]
                 (let [role (get m "role")
                       out (transient [])]
                   (when (= role "tool")
                     (conj! out (str "[tool result]\n" (cut (get m "content")))))
                   (when (get m "content")
                     (conj! out (str "[" role "]\n" (get m "content"))))
                   (doseq [c (get m "tool_calls")]
                     (conj! out (str "[tool call] " (get-in c ["function" "name"])
                                      " " (cut (get-in c ["function" "arguments"])))))
                   (persistent! out)))
               messages))))

(defn- parse-sse-line
  [^String line]
  (let [t (str/trim line)]
    (when (str/starts-with? t "data:")
      (let [data (str/trim (subs t 5))]
        (when (not= data "[DONE]")
          (try (json/read-str data) (catch Exception _ nil)))))))

(defn collect-chunks
  "Assemble streamed chunks (or one non-streamed map) into [message usage].
   Emits thinking/text via emit fn."
  [chunks emit]
  (let [content (StringBuilder.)
        reasoning (StringBuilder.)
        reasoning-key (atom nil)
        details (atom [])
        calls (atom [])
        usage (atom nil)]
    (doseq [chunk chunks]
      (when (get chunk "error")
        (throw (api-error (json/write-str (get chunk "error")) {:chunk chunk})))
      (when (get chunk "usage") (reset! usage (get chunk "usage")))
      (doseq [choice (or (get chunk "choices") [])]
        (let [delta (or (get choice "delta") (get choice "message") {})]
          ;; thinking: reasoning_content | reasoning | reasoning_details parts
          (let [thought (or (when (string? (get delta "reasoning_content"))
                              (when (seq (get delta "reasoning_content"))
                                (reset! reasoning-key "reasoning_content")
                                (get delta "reasoning_content")))
                            (when (string? (get delta "reasoning"))
                              (when (seq (get delta "reasoning"))
                                (reset! reasoning-key (or @reasoning-key "reasoning"))
                                (get delta "reasoning"))))
                parts (filter map? (or (get delta "reasoning_details") []))]
            (doseq [p parts]
              (let [i (int (or (get p "index") (count @details)))]
                (swap! details
                       (fn [d]
                         (let [d (if (<= (count d) i)
                                   (into d (repeat (inc (- i (count d))) {}))
                                   d)]
                           (assoc d i
                                  (reduce (fn [acc [k v]]
                                            (if (and (contains? #{"text" "summary"} k) (string? v))
                                              (update acc k str v)
                                              (if (some? v) (assoc acc k v) acc)))
                                          (nth d i) p)))))))
            (let [detail-text (str/join "" (keep #(or (get % "text") (get % "summary"))
                                                 (filter #(or (string? (get % "text"))
                                                              (string? (get % "summary")))
                                                         parts)))
                  thought (or thought (when (seq detail-text)
                                        (when-not @reasoning-key
                                          (reset! reasoning-key "reasoning"))
                                        detail-text))]
              (when (and thought (seq thought))
                (.append reasoning thought)
                (emit "reasoning" thought)))
            (when (get delta "content")
              (.append content (get delta "content"))
              (emit "text" (get delta "content")))
            (doseq [tc (or (get delta "tool_calls") [])]
              (let [idx (get tc "index")
                    cur @calls
                    i (if (some? idx)
                        (int idx)
                        (if (or (empty? cur)
                                (and (get tc "id")
                                     (not= (get tc "id") (get (last cur) "id"))))
                          (count cur)
                          (dec (count cur))))]
                (swap! calls
                       (fn [cs]
                         (let [cs (if (<= (count cs) i)
                                    (into cs (repeat (inc (- i (count cs)))
                                                     {"id" "" "type" "function"
                                                      "function" {"name" "" "arguments" ""}}))
                                    cs)
                               call (nth cs i)
                               f (or (get tc "function") {})
                               nm (or (get f "name") "")
                               ag (get f "arguments")
                               ags (cond (nil? ag) ""
                                         (string? ag) ag
                                         :else (json/write-str ag))]
                           (assoc cs i
                                  (-> call
                                      (assoc "id" (or (get tc "id") (get call "id") ""))
                                      (assoc-in ["function" "name"]
                                                (str (get-in call ["function" "name"]) nm))
                                      (update-in ["function" "arguments"] str ags))))))))))))
    (let [cs @calls
          cs (vec (map-indexed (fn [n c]
                                 (if (seq (get c "id")) c
                                     (assoc c "id" (str "call_" (System/nanoTime) "_" n))))
                               cs))
          msg (cond-> {"role" "assistant"
                       "content" (let [s (str content)]
                                   (if (seq s) s (when (empty? cs) "")))}
                (seq cs) (assoc "tool_calls" cs)
                (pos? (.length reasoning)) (assoc (or @reasoning-key "reasoning") (str reasoning))
                (seq @details) (assoc "reasoning_details" @details))]
      [msg @usage])))

(defn- do-request
  [cfg body]
  (let [client (-> (HttpClient/newBuilder)
                   (.connectTimeout (Duration/ofSeconds 30))
                   .build)
        headers (cond-> {"Content-Type" "application/json"}
                  (seq (get cfg "headers")) (merge (get cfg "headers"))
                  (seq (get cfg "api_key")) (assoc "Authorization" (str "Bearer " (get cfg "api_key"))))
        url (str (str/replace (get cfg "base_url") #"/$" "") "/chat/completions")
        payload (json/write-str body)
        builder (-> (HttpRequest/newBuilder (URI/create url))
                    (.timeout (Duration/ofSeconds (long (or (get cfg "timeout") 600))))
                    (.POST (HttpRequest$BodyPublishers/ofString payload)))
        _ (doseq [[k v] headers] (.header builder k (str v)))
        req (.build builder)
        resp (.send client req (HttpResponse$BodyHandlers/ofInputStream))
        status (.statusCode resp)
        ctype (str (or (.orElse (.firstValue (.headers resp) "Content-Type") "") ""))]
    {:status status :content-type ctype :stream (.body resp)}))

(defn- read-all
  [^java.io.InputStream in]
  (with-open [r (BufferedReader. (InputStreamReader. in "UTF-8"))]
    (let [sb (StringBuilder.) line (atom nil)]
      (loop []
        (reset! line (.readLine r))
        (when @line (do (.append sb @line) (.append sb "\n") (recur))))
      (str sb))))

(defn chat
  "One streaming chat completion. Returns [assistant-message usage-or-nil].
   emit is (fn [kind text]). extra merges into body (e.g. tool_choice)."
  [cfg messages tool-schemas emit & {:keys [extra] :or {extra {}}}]
  (let [drop? (not (get cfg "keep_reasoning"))
        body (clean-body
              (merge {"model" (get cfg "model")
                      "messages" (strip-reasoning messages (not drop?))
                      "tools" tool-schemas
                      "stream" true
                      "stream_options" {"include_usage" true}
                      "temperature" (get cfg "temperature")
                      "max_tokens" (get cfg "max_tokens")}
                     extra
                     (or (get cfg "extra_body") {})))
        ;; a null value in extra_body removes the key
        body (into {} (filter (fn [[_ v]] (some? v)) body))]
    (loop [attempt 0]
      (let [delay-s (min (bit-shift-left 1 attempt) 30)]
        (let [outcome
              (try
                (let [{:keys [status content-type stream]} (do-request cfg body)]
                  (cond
                    (= status 413) {:throw (context-full (str "HTTP 413: context too large"))}
                    (and (contains? #{400 422} status))
                    {:defer :check-overflow :stream stream}
                    (or (= status 408) (= status 409) (= status 429) (>= status 500))
                    {:defer :retry :problem (str "HTTP " status) :stream stream}
                    (>= status 400)
                    {:defer :read-error :stream stream :status status}
                    (str/includes? content-type "event-stream")
                    (let [chunks (with-open [r (BufferedReader. (InputStreamReader. stream "UTF-8"))]
                                   (loop [acc []]
                                     (let [line (.readLine r)]
                                       (if (nil? line)
                                         acc
                                         (if-let [c (parse-sse-line line)]
                                           (recur (conj acc c))
                                           (recur acc))))))]
                      {:return (collect-chunks chunks emit)})
                    :else
                    {:return (collect-chunks [(json/read-str (read-all stream))] emit)}))
                (catch java.io.IOException e
                  {:defer :retry :problem (str "IOException: " (.getMessage e))})
                (catch java.net.http.HttpTimeoutException e
                  {:defer :retry :problem (str "timeout: " (.getMessage e))})
                (catch clojure.lang.ExceptionInfo e
                  (if (or (context-full? e) (= ::api-error (:type (ex-data e))))
                    {:throw e}
                    {:defer :retry :problem (str "error: " (.getMessage e))}))
                (catch Exception e
                  {:defer :retry :problem (str (.getSimpleName (class e)) ": " (.getMessage e))}))]
          (cond
            (:return outcome) (:return outcome)
            (:throw outcome) (throw (:throw outcome))
            :else
            (let [kind (:defer outcome)]
              (cond
                (= kind :check-overflow)
                (let [detail (try (subs (read-all (:stream outcome)) 0 2000)
                                  (catch Exception _ ""))]
                  (if (re-find overflow-pattern detail)
                    (throw (context-full detail))
                    (throw (api-error (str "HTTP error: " detail) {:status 400}))))
                (= kind :read-error)
                (let [detail (try (subs (read-all (:stream outcome)) 0 2000)
                                  (catch Exception _ ""))]
                  (throw (api-error (str "HTTP " (:status outcome) ": " detail)
                                    {:status (:status outcome)})))
                :else
                (do
                  (try (.close ^java.io.InputStream (:stream outcome)) (catch Exception _))
                  (when (>= attempt 5)
                    (throw (api-error (str "giving up after repeated failures (" (:problem outcome) ")") {})))
                  (emit "reasoning" "")
                  (Thread/sleep (* 1000 delay-s))
                  (recur (inc attempt)))))))))))
