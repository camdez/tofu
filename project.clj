(defproject tofu "1.0.0-SNAPSHOT"
  :description "A simple command line to-do list manager."
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [jline/jline "3.0.0.M1"]]
  :profiles {:dev {:dependencies [[midje "1.9.9"]]}}
  :main tofu.core)
