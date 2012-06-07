(defproject fungp "0.1"
  :description "Genetic programming in Clojure"
  :main fungp.sample
  :aot :all
  :dev-dependencies [[lein-marginalia "0.7.0"]]
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/math.numeric-tower "0.0.1"]]
  :plugins      [[lein-swank "1.4.4"]])
