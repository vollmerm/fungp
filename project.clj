(defproject fungp "0.3.1"
  :description "Genetic programming in Clojure"
  :jvm-opts ["-XX:ReservedCodeCacheSize=128m" "-server"]
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :aot [fungp.java-api]
  :url "http://gaia.ecs.csus.edu/~vollmerm/gp/"
  :license {:name "GNU General Public License v3"
            :url "http://www.gnu.org/licenses/gpl.html"}
  ;; Uncomment the line below to have lein compile
  ;; the sample Java class that uses the Java API:
  ;; :java-source-paths ["src/fungp/java"]
  :plugins [[lein-marginalia "0.7.1"]])
