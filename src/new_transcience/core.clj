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
           (POST "/saveBlocks" [blocks]
                 (spit "resources/blocks" blocks)
                 "nice"
                 )
           (GET "/blocks" []
                (slurp "resources/blocks"))

           (route/not-found "<h1>Page not found</h1>"))
(def app
  (handler/site app-routes))



