(defproject fungp "0.3.2"
  :description "Genetic programming in Clojure"
  :jvm-opts ["-XX:ReservedCodeCacheSize=128m" "-server"]
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :url "http://gaia.ecs.csus.edu/~vollmerm/gp/"
  :license {:name "GNU General Public License v3"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :plugins [[lein-marginalia "0.7.1"]])
