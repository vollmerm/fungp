;;; Sample use
;;; ----------

(ns fungp.sample
  "Sample use of fungp."
  (:use fungp.core)
  (:use fungp.util))

;;; ### Examples of functions to use
;;; 
;;; Safe versions of inverse and divide to avoid divide-by-zero errors.

(defn inv
  "Inverts a number. If it's 0, return 0"
  [x]
  (if (zero? x) 0
      (/ 1 x)))

(defn sdiv
  "Divide two numbers. Returns 0 on attempt to divide by 0"
  [x y]
  (if (zero? y) 0
      (/ x y)))

;;; java.lang.Math methods cannot be higher-order functions, so they must
;;; be wrapped in clojure functions.

(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn tan [x] (Math/tan x))
(defn abs [x] (if (< x 0) (* -1 x) x))

;;; Conditionals can be done in terms of 4 arity functions

(defn ifeq [a b c d] (if (= a b) c d))
(defn ifnoteq [a b c d] (if (not (= a b)) c d))
(defn gte [a b c d] (if (>= a b) c d))

;;; ### Test run

(def functions
  "Functions and their arities"
  '[[+ 2][- 2][* 2][Math/sin 1][Math/cos 1][inc 1]])

(def terminals
  "Terminals to be used as leaves"
  ['a])

(def proto-code (list 'fn '[x] '(+ x (- (Math/sin x) (* x x)))))

(def desired-function (eval proto-code))

(def test-range [-5.0 -1.0 -0.5 0 0.5 1.0 5.0])

(def actual (map desired-function test-range))

(defn fitness
  "Fitness function. Takes a tree, evals it, and returns a fitness/error score."
  [tree]
  (let [f (eval (list 'fn '[a] tree))
        results (map f test-range)]
    (reduce + (map off-by-sq actual results))))

(defn report
  "Reporting function. Prints out the tree and its score"
  [tree fitness]
  (print "Code:\t")(print (list 'fn '[a] tree))(print "\n")
  (print "Error:\t")(print fitness)(print "\n\n")
  (flush))

(defn test-genetic-program
  "The main test program. Takes iteration and migration counts as parameters"
  [n1 n2]
  (println "\nfungp :: Functional Genetic Programming in Clojure")
  (println "Mike Vollmer, 2012")
  (println "\n==================================================\n")
  (print "Matching function: ")(print proto-code)(print "\n")
  (print "On inputs: ")(print test-range)(print "\n")
  (println "\n==================================================\n")
  (let [options {:iterations n1 :migrations n2 :num-islands 4 :population-size 250 :tournament-size 5 :mutation-probability 0.1
                 :mutation-depth 4 :max-depth 25 :terminals terminals :fitness fitness :functions functions :report report}
        [tree score] (rest (run-genetic-programming options))]
    (do (println "Done!")
        (report tree score)
        (eval (list 'fn '[a] tree)))))
