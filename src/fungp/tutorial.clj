;;; Getting Started Tutorial
;;; ------------------------
;;;
;;; The simplest problem for GP is regression, or coming up
;;; with a function to map pairs of input and output. This tutorial goes through
;;; how to set up *fungp* to evolve a function to match a series of points.

(ns fungp.tutorial
  "Sample use of fungp."
  (:use fungp.core)
  (:use fungp.util)
  (:use clojure.pprint))

;;; ### Choosing functions and terminals
;;;
;;; The trees evolved in *fungp* consist of functions and terminals. The functions
;;; take an arbitrary number of arguments.
;;;
;;; For example, the expression:
;;;
;;;     (+ a (* b c))
;;;
;;; Is a tree with two functions: + and *. Each function takes two parameters.
;;; Functions can take the result of other functions as parameters, as well as
;;; terminals.

(def sample-functions
  "Here's a vector of vectors consisting of [symbol arity] pairs. The symbol must resolve
   to a function, and the arity number is an integer representing how many arguments
   that function takes."
  '[[+ 2][- 2][* 2][fungp.util/abs 1]
    [fungp.util/sdiv 2][inc 1][dec 1]])

(def sample-parameters
  "This defines the parameters (or, in this case, parameter) to be used to eval the
  generated functions."
  ['a])

(def number-literals
  "This generates floating point numbers up to 10 as number literals to be used in the code."
  (map float (range 10)))

;;; ### Testing data
;;;
;;; The following code is used to test the evolved functions.

(def test-range
  "This defines the range of input to use as test input. The first argument for map here uses a shortcut for
   single-variable anonymous functions in Clojure."
  (map #(* 2 (- % 5)) (range 10)))

(defn match-func
  "For sake of convenience, we can define a function to generate the outputs we're attempting to match."
  [x] (abs (* 3 (* x x))))

(def sample-actual
  "This defines the actual outputs we are trying to match."
  (map float (map match-func test-range)))

;;; ### Computing and reporting fitness

(defn sample-fitness
  "The fitness function; it takes a tree, evals it, and returns a fitness/error score."
  [tree]
  (try
    (let [f (compile-tree tree sample-parameters) ;; compile using compile-tree
          results (map f test-range)] ;; map the function to the test range
      ;; then we compare the test results to the actual expected output
      ;; off-by-sq is a utility function that calculates difference squared
      (reduce + (map off-by-sq sample-actual results)))
    ;; not necessary here, but this is how you might catch a possible exception
    (catch Exception e (println e) (println tree))))

(defn sample-report
  "Reporting function. Prints out the tree and its score"
  [tree fitness]
  (pprint tree)
  (println (str "Error:\t" fitness "\n"))
  (flush))

;;; ### Running it!

(defn test-genetic-program
  "This is the function that launches *fungp* and starts the evolution. It takes iteration and migration counts as parameters."
  [n1 n2]
  (println "\nfungp :: Functional Genetic Programming in Clojure")
  (println "Mike Vollmer, 2012")
  (println (str "Test inputs: " (vec test-range)))
  (println (str "Test outputs: " (vec sample-actual)))
  (println (str "Max generations: " (* n1 n2)))
  (println)
  ;; These keyword arguments specify the options for fungp. They should be self-explanatory,
  ;; but you can read more about them in fungp.core
  (let [options {:iterations n1 :migrations n2 :num-islands 4 :population-size 50
                 :tournament-size 5 :mutation-probability 0.1
                 :max-depth 10 :terminals sample-parameters
                 :numbers number-literals :fitness sample-fitness
                 :functions sample-functions :report sample-report }
        [tree score] (rest (run-genetic-programming options))]
    (do (println "Done!")
        (sample-report tree score))))

;;; To run *test-genetic-program* you can load this file in the repl and run the function:
;;;
;;;    user=> (use 'fungp.tutorial)
;;;    nil
;;;    user=> (test-genetic-program 15 15)
;;;    ...
