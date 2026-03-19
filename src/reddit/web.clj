(ns reddit.web
  "Authenticated web interactions with Reddit (comment, submit, vote)."
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [reddit.cookies :as cookies])
  (:import [java.net URLEncoder]))

(def ^:private ua
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36")

(def ^:private cookie-jar (atom nil))
(def ^:private modhash (atom nil))

(defn- init-cookies! []
  (when-not @cookie-jar
    (let [cks (cookies/get-cookies ".reddit.com")
          f (java.io.File/createTempFile "reddit-cookies" ".txt")]
      (.deleteOnExit f)
      (with-open [w (io/writer f)]
        (.write w "# Netscape HTTP Cookie File\n")
        (doseq [{:keys [name value path host]} cks]
          (when (seq value)
            (.write w (str host "\t"
                          (if (str/starts-with? host ".") "TRUE" "FALSE") "\t"
                          path "\t"
                          "TRUE\t"
                          "0\t"
                          name "\t"
                          value "\n")))))
      (reset! cookie-jar (.getAbsolutePath f))
      (binding [*out* *err*]
        (println (str "Loaded " (count cks) " cookies for reddit.com"))))))

(defn- fetch-modhash! []
  (when-not @modhash
    (init-cookies!)
    (let [pb (ProcessBuilder. ["curl" "-sSL"
                                "-b" @cookie-jar "-c" @cookie-jar
                                "-H" (str "User-Agent: " ua)
                                "https://old.reddit.com/api/me.json"])
          proc (.start pb)
          out (str/trim (slurp (.getInputStream proc)))
          _ (.waitFor proc)
          data (when (seq out) (json/read-str out :key-fn keyword))
          mh (get-in data [:data :modhash])]
      (if (seq mh)
        (do (reset! modhash mh)
            (binding [*out* *err*]
              (println "Got modhash for" (get-in data [:data :name]))))
        (throw (ex-info "Could not get modhash - make sure you're logged in to Reddit in Chrome" {}))))))

(defn- api-post [endpoint params]
  (init-cookies!)
  (fetch-modhash!)
  (let [body-file (java.io.File/createTempFile "reddit-resp" ".json")
        form-data (str/join "&"
                    (map (fn [[k v]]
                           (str (URLEncoder/encode (name k) "UTF-8") "="
                                (URLEncoder/encode (str v) "UTF-8")))
                         (assoc params :uh @modhash :api_type "json")))
        args ["curl" "-sSL"
              "-b" @cookie-jar "-c" @cookie-jar
              "-H" (str "User-Agent: " ua)
              "-X" "POST"
              "-d" form-data
              "-o" (.getAbsolutePath body-file)
              "-w" "%{http_code}"
              (str "https://old.reddit.com/api/" endpoint)]
        pb (ProcessBuilder. ^java.util.List args)
        proc (.start pb)
        status-str (str/trim (slurp (.getInputStream proc)))
        err (str/trim (slurp (.getErrorStream proc)))
        exit (.waitFor proc)
        body (slurp body-file)]
    (.delete body-file)
    (when-not (zero? exit)
      (binding [*out* *err*]
        (println "curl error:" err)))
    {:status (or (parse-long status-str) 0)
     :body body
     :data (try (json/read-str body :key-fn keyword) (catch Exception _ nil))}))

(defn reply-comment
  "Reply to a comment or post on Reddit. thing-id is the fullname (t1_ or t3_ prefixed)."
  [thing-id message]
  (let [resp (api-post "comment" {:thing_id thing-id :text message})
        errors (get-in resp [:data :json :errors])]
    (if (and (seq errors) (not= errors []))
      (throw (ex-info (str "Reddit API error: " (pr-str errors)) {:errors errors}))
      (str "Reply posted successfully to " thing-id "."))))

(defn submit-post
  "Submit a new post to a subreddit. kind is 'self' or 'link'."
  [subreddit title kind & {:keys [text url]}]
  (let [params (cond-> {:sr subreddit :title title :kind kind}
                 text (assoc :text text)
                 url (assoc :url url))
        resp (api-post "submit" params)
        errors (get-in resp [:data :json :errors])]
    (if (and (seq errors) (not= errors []))
      (throw (ex-info (str "Reddit API error: " (pr-str errors)) {:errors errors}))
      (let [post-url (get-in resp [:data :json :data :url])]
        (str "Post submitted successfully."
             (when post-url (str " URL: " post-url)))))))
