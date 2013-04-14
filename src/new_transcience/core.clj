(ns new-transcience.core
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defroutes app-routes

           (route/files "/" {:root "www"})
           (POST "/saveThings" [things level]
                 (spit (str "resources/things" "-" level) things)
                 "nice"
                 )
           (GET "/things" [level]
                (println "getting data for " level)
                (slurp (str "resources/things-" level)))

           (route/not-found "<h1>Page not found</h1>"))
(def app
  (handler/site app-routes))


