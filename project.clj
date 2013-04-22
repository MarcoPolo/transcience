(defproject new-transcience "0.1.0-SNAPSHOT"
  :description "Small and fun Physics game"
  :url "http://transga.me"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :cljsbuild
  {:builds
   [{:source-paths ["src-cljs"]
     :compiler
     {:pretty-print true
      :output-to "www/main.js"
      :optimizations :simple}}]}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [jayq "2.3.0"]
                 [compojure "1.1.5"]]
  :plugins [[lein-cljsbuild "0.3.0"]
            [lein-ring "0.8.3"]]
  :ring {:handler new-transcience.core/app}
  :profiles
  {:dev {:dependencies [[ring-mock "0.1.3"]]}})
