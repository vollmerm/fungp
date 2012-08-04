;;; Sample use
;;; ----------

(ns fungp.sample
  "Sample use of fungp."
  (:use fungp.core)
  (:use fungp.util)
  (:require clojure.pprint))



;;; ### Test run

(def sample-functions
  "Functions and their arities"
  '[[+ 2][- 2][* 2][fungp.util/abs 1]
    [fungp.util/sdiv 2][inc 1][dec 1]])

(def sample-parameters
  "Parameters to be used to eval the generated functions"
  ['a])

(def number-literals
  "Number literals to be used in the code"
  (map float (range 10)))

(def test-range (map #(* 2 (- % 5)) (range 10)))

;(defn match-func [x] (- (* 0.1 (abs x)) (sin x)))
(defn match-func [x] (abs (* 3 (* x x))))

(def sample-actual (map float (map match-func test-range)))

(defn sample-fitness
  "Fitness function. Takes a tree, evals it, and returns a fitness/error score."
  [tree]
  (try
    (let [f (eval (list 'fn 'testfunc '[a] tree))
          results (map f test-range)]
      (reduce + (map off-by-sq sample-actual results)))
    (catch Exception e (println e) (println tree))))

(defn sample-report
  "Reporting function. Prints out the tree and its score"
  [tree fitness]
  (clojure.pprint/pprint (list 'fn '[a] tree))(print "\n")
  (print "Error:\t")(print fitness)(print "\n\n")
  (flush))

(defn test-genetic-program
  "The main test program. Takes iteration and migration counts as parameters"
  [n1 n2]
  (println "\nfungp :: Functional Genetic Programming in Clojure")
  (println "Mike Vollmer, 2012")
  (print "Test inputs: ")(print test-range)(print "\n")
  (print "Test outputs: ")(print sample-actual)(print "\n\n")
  (let [options {:iterations n1 :migrations n2 :num-islands 4 :population-size 100 :tournament-size 5 :mutation-probability 0.1
                 :max-depth 10 :terminals sample-parameters :numbers number-literals :fitness sample-fitness
                 :functions sample-functions :report sample-report}
        [tree score] (rest (run-genetic-programming options))]
    (do (println "Done!")
        (sample-report tree score))))
