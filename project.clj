(defproject fungp "0.3"
  :description "Genetic programming in Clojure"
  :jvm-opts ["-XX:ReservedCodeCacheSize=128m" "-server"]
  :dev-dependencies [[lein-marginalia "0.7.1"]]
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :aot [fungp.java-api]
  ;; Uncomment the line below to have lein compile
  ;; the sample Java class that uses the Java API:
  ;; :java-source-paths ["src/fungp/java"]
  )
