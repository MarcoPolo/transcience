(ns new-transcience.core
  (:use [jayq.core :only [$ css inner]])
  (:require [clojure.browser.repl :as repl]
             [new-transcience.engine :as engine]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(.log js/console "hello world")

(inner ($ :#stuff) "Changed from cljs!")

;; So we can connect to the repl server
(repl/connect "http://localhost:9001/repl")

(def ball (engine/create-circle {:color "black" }))

(comment

  (+ 1 2)

  )



