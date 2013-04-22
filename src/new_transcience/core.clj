(ns new-transcience.core
  (:use compojure.core)
  (:import [java.security MessageDigest])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]))

(def directory-name "resources/levels")
(def directory (clojure.java.io/file directory-name))

(defn hash-str [input] 
  (let [hash (MessageDigest/getInstance "SHA-256")]
    (. hash update (.getBytes input))
    (let [digest (.digest hash)]
      (apply str (map #(format "%02x" (bit-and % 0xff)) digest)))))

(defn hash-exists? [hash]
  (seq (filter #(= hash (.getName %)) (file-seq directory))))

(defn save-level! [things]
  (let [hash (subs (hash-str (str (rand))) 59)]
    (if (hash-exists? hash)
      (save-level! things)
      (do 
        (spit (str directory-name "/" hash) things)
        hash))))

(defn valid-hash? [hash]
  (try (Long/parseLong "AFF" 16) (catch Exception e false)))


(defroutes app-routes

           (route/files "/" {:root "www"})
           (POST "/saveThings" [things]
                 (println "things are" things)
                 (save-level! things))
           (GET "/random" [] 
              (let [level (rand-nth (rest (file-seq directory)))]
                (str "{ \"hash\":\"" (.getName level) "\", \"things\":" (slurp (.getPath level)) "}")))
           (GET "/things" [level]
                (if (valid-hash? level)
                  (slurp (str directory-name "/" level))
                  {:error "Wrong Hash"}))
           (route/not-found "<h1>Page not found</h1>"))
(def app
  (handler/site app-routes))

           (file-seq directory)

