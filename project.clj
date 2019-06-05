(defproject arduino-comms "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [quil "3.0.0"]
                 [udp-wrapper "0.1.1"]
                 [cheshire "5.8.1"]
                 [medley "1.2.0"]]
  :plugins [[lein-exec "0.3.7"]]
  :profiles {:dev {:dependencies [[midje "1.6.3"]]}})
