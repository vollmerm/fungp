(defproject fungp "0.2"
  :description "Genetic programming in Clojure"
  :jvm-opts ["-XX:ReservedCodeCacheSize=128m" "-server"]
  :dev-dependencies [[lein-marginalia "0.7.1"]]
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :aot [fungp.java-api fungp.core]
  :java-source-paths ["src/fungp/java"]
  :plugins      [[lein-swank "1.4.4"]])
