;;; Getting Started Tutorial
;;; ------------------------
;;;
;;; The simplest problem for GP is regression, or coming up
;;; with a function to map pairs of input and output. This tutorial goes through
;;; how to set up *fungp* to evolve a function to match a series of points.
;;;
;;; If you don't already have *fungp* set up, look at the explanation at the top
;;; of fungp.core. Basically, all you need to do is install lein and download the
;;; *fungp* code.

(ns fungp.tutorial
  "Namespace for sample use of fungp."
  (:use fungp.core) ;; include the core framework
  (:use fungp.util) ;; include utility functions
  (:use clojure.pprint))

;;; The general structure here is very simple, and probably similar to what you'll want
;;; if you're using *fungp.* We define a bunch of values and use them at the bottom
;;; when invoking the library. In creating these values we have to think a little bit
;;; about the problem we're trying to solve.
;;;
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

;;; We also need to pick the terminals. In the above expression, a b and c were terminals.
;;; Terminals can be anything, but *fungp* separates them into two groups: numbers and
;;; everything else. This is just a suggestion, though, not a requirement. You can
;;; put anything you want in your ordinary terminals list, including numbers.
;;;
;;; For this example, the only terminals are the function parameter (which I'll call a)
;;; and a range of numbers.

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

(def training-range
  "This defines the range of input to use as training input. The first argument for map here uses a shortcut for
   single-variable anonymous functions in Clojure."
  (map #(* 2 (- % 5)) (range 10)))

(defn match-func
  "For sake of convenience, we can define a function to generate the outputs we're attempting to match."
  [a] (abs (* 3 (* a a))))

(def actual-output
  "This defines the actual outputs we are trying to match."
  (map float (map match-func training-range)))

;;; ### Computing and reporting fitness

(defn sample-fitness
  "The fitness function; it takes a tree, evals it, and returns a fitness/error score."
  [tree]
  (try
    (let [f (compile-tree tree sample-parameters) ;; compile using compile-tree
          results (map f training-range)] ;; map the function to the test range
      ;; then we compare the test results to the actual expected output
      ;; off-by-sq is a utility function that calculates difference squared
      (reduce + (map off-by-sq actual-output results)))
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
  (println (str "Test inputs: " (vec training-range)))
  (println (str "Test outputs: " (vec actual-output)))
  (println (str "Max generations: " (* n1 n2)))
  (println)
  ;; These keyword arguments specify the options for fungp. They should be self-explanatory,
  ;; but you can read more about them in fungp.core
  (let [options {:iterations n1 :migrations n2 :num-islands 6 :population-size 40
                 :tournament-size 5 :mutation-probability 0.1
                 :max-depth 10 :terminals sample-parameters
                 :numbers number-literals :fitness sample-fitness
                 :functions sample-functions :report sample-report }
        ;; the data returned by run-genetic-programming is as follows:
        ;; [population [best-tree score]]
        ;; since we don't care about keeping the whole population
        ;; around, we can save off the tree and score like this
        [tree score] (rest (run-genetic-programming options))]
    ;; that's it!
    (do (println "Done!")
        (sample-report tree score))))

;;; To run *test-genetic-program* you can load this file in the repl and run the function:
;;;
;;;     user=> (use 'fungp.tutorial)
;;;     nil
;;;     user=> (test-genetic-program 15 15)
;;;     ...
;;;
;;; The two parameters correspond to the iterations between migrations and the number of
;;; migrations. This also results in the reporting rate -- results are printed to the
;;; screen at each migration. The total number of generations is the result of these
;;; two numbers multiplied together.
