(defproject clojure101 "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]
                     [lein-search "0.3.3"]
                     [criterium "0.0.1-SNAPSHOT"]]
  :jvm-flags ["-XX:+UseConcMarkSweepGC" "-Xmx1g" "-XX:+UseCompressedOops"])
