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

(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn tan [x] (Math/tan x))
(defn abs [x] (if (< x 0) (* -1 x) x))
(defn dub [x] (* x x))
(defn half [x] (sdiv x 2))
(defn sqrt [x] (if (x > 0) (Math/sqrt x) 0))


;;; Conditionals can be done in terms of 4 arity functions

(defn ifeq [a b c d] (if (= a b) c d))
(defn ifnoteq [a b c d] (if (not (= a b)) c d))
(defn gte [a b c d] (if (>= a b) c d))

(defn gt [x y] (if (> x y) 1 -1))

;;; ### Test run

(def functions
  "Functions and their arities"
  '[[+ 2][- 2][* 2][fungp.sample/abs 1]
    [sdiv 2][inc 1][dec 1]])

(def parameters
  "Parameters to be used to eval the generated functions"
  ['a])

(def parameter-weight 5)

(def number-literals
  "Number literals to be used in the code"
  (map float (range 10)))

(def number-weight 1)

(defn duplicate-symbol
  [s n] (if (zero? n) s
            (concat s (duplicate-symbol s (- n 1)))))

(def terminals
  "Terminals to be used as leaves"
  (concat (duplicate-symbol number-literals number-weight)
          (duplicate-symbol parameters parameter-weight)))

(def test-range (map #(* 2 (- % 5)) (range 10)))

;(defn match-func [x] (- (* 0.1 (abs x)) (sin x)))
(defn match-func [x] (abs (* 3 (* x x))))

(def actual (map float (map match-func test-range)))

(defn fitness
  "Fitness function. Takes a tree, evals it, and returns a fitness/error score."
  [tree]
  (try
    (let [f (eval (list 'fn 'testfunc '[a] tree))
          results (map f test-range)]
      (reduce + (map off-by-sq actual results)))
    (catch Exception e (println e) (println tree))))

(defn report
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
  (print "Test outputs: ")(print actual)(print "\n\n")
  (let [options {:iterations n1 :migrations n2 :num-islands 4 :population-size 100 :tournament-size 5 :mutation-probability 0.1
                 :max-depth 35 :terminals terminals :fitness fitness :functions functions :report report
                 :adf-count 0}
        [tree score] (rest (run-genetic-programming options))]
    (do (println "Done!")
        (report tree score)
        (eval (list 'fn '[a] tree)))))
