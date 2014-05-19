(ns fungp.util)

;;; ### Useful functions for evolved code

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
(defn abs [x] (if (>= x 0) x (- x)))
(defn dub [x] (* x 2))
(defn half [x] (sdiv x 2))
(defn sqrt [x] (if (> x 0) (Math/sqrt x) 0))

;;; Conditionals can be done in terms of functions

(defn ifeq [a b c d] (if (= a b) c d))
(defn ifnoteq [a b c d] (if (not (= a b)) c d))
(defn gte [a b c d] (if (>= a b) c d))
(defn gt [x y] (if (> x y) 1 -1))

;;; ### Misc. manipulation/utility functions

(defn max-tree-height
  "Find the maximum height of a tree. The max height is the distance from the root to the
   deepest leaf."
  [tree]
  (if (not (seq? tree)) 0
    (+ 1 (reduce max (map max-tree-height tree)))))

(defn valid-tree?
  "Checks the type on a tree to make sure it's a list, symbol, or number."
  [tree] (or (list? tree) (symbol? tree) (number? tree)))

(defn compile-tree
  "Compiles a tree into a Clojure function, and thus into JVM bytecode. Takes a tree
   and the parameters the function should have."
  [tree parameters] (eval (list 'fn parameters tree)))


(defn flip
  "Convenience function. Generates true with a probablilty of the
   given parameter (a number between 0 and 1)"
  [chance] (< (rand) chance))

(defn find-op
  "Find the entry in the function sequence for a given operator."
  [op funcs] (first (filter (fn [x] (= (:op x) op)) funcs)))

(defn average
  [list]
  (let [sum (reduce + list)
        length (count list)]
    (/ sum length)))

(defn off-by
  "Calculate error."
  [x y] (abs(- x y)))

(defn off-by-sq
  "Calculate error squared."
  [x y] (let [error (off-by x y)]
          (* error error)))

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defmacro do-loop
  "The macro used to define loops. Used by ADLs. Takes four code branches and one integer."
  [init break body update limit]
    `(do ~init
       (loop [loop-counter# ~limit value# 0]
         (if (or (zero? loop-counter#)
                 (< ~break 0))
           value#
           (let [newvalue# ~body]
             (do ~update
               (recur (dec loop-counter#) newvalue#)))))))

(defn generic-report
  "Reporting function. Prints out the tree and its score"
  [tree fitness]
  (println tree)
  (println (str "Error:\t" fitness "\n"))
  (flush))
