(ns new-transcience.core
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]))

(defroutes app-routes

           (route/files "/" {:root "www"})
           (POST "/saveThings" [things level]
                 (spit (str "resources/things" "-" (Integer. level)) things)
                 "nice"
                 )
           (GET "/things" [level]
                (println "getting data for " level)
                (slurp (str "resources/things-" (Integer. level))))

           (route/not-found "<h1>Page not found</h1>"))
(def app
  (handler/site app-routes))
