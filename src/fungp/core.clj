;;; Genetic programming in clojure
;;; ==============================
;;;
;;; Mike Vollmer, 2012, GPL
;;;
;;; What is this?
;;; -------------
;;;
;;; **fungp** is a parallel genetic programming library implemented in the
;;; Clojure programming language. The "fun" comes from functional, and because
;;; genetic programming can be fun! Also I'm bad at naming things.
;;;
;;; > There are only two hard things in Computer Science: cache invalidation,
;;; > naming things, and off-by-one errors.
;;; >
;;; > --- *Paraphrased from Phil Karlton*
;;; 
;;; The library is in its early stages right now, but it's usable. Currently it
;;; has the following features:
;;;
;;;  * Custom evaluation and reporting logic via function parameters
;;;  * Training data can be 
;;;  * Parallelism: subpopulations run in native threads
;;;  * Evolve and test functions of multiple arities
;;;
;;; How do I use it?
;;; ----------------
;;;
;;; Call the **run-gp** function. See the source code for the full list of 
;;; keyword parameters.
;;;
;;; Here's an example:
;;;
;;;      (run-gp {:gens iter :cycles cycle
;;;               :pop-size 6 :forest-size 50
;;;               :symbols symbols :funcs funcs
;;;               :const [1 -1] :depth [2 1]
;;;               :rep [repfunc 1] :mutation 0.1 :tournament 3
;;;               :test [testfit testdata]}))
;;;
;;; Functions are defined as a sequence of maps, each having keys :op,
;;; :arity, and :name. :op is for the function, :arity is the number
;;; of arguments, and :name is the symbol used to print it out if it's
;;; in the answer at the end (you probably want it to be the same as the
;;; name of the function).
;;;
;;; Here's an example:
;;;
;;;      [{:op *   :arity 2 :name '*}
;;;       {:op +   :arity 2 :name '+}
;;;       {:op sin :arity 1 :name 'sin}]
;;;
;;; TODO: write code to pass options map (and validate it), rather than using run-gp closure


(ns fungp.core
  (:require [clojure.math [numeric-tower :as math]]))

(defn flip
  "Convenience function. Generates true with a probablilty of the
   given parameter (a number between 0 and 1)"
  [chance] (< (rand) chance))

(defn find-op
  "Find the entry in the function sequence for a given operator."
  [op funcs] (first (filter (fn [x] (= (:op x) op)) funcs)))

(defn conv-code
  "Take a tree and return a list of symbols based on the :name symbols."
  [tree funcs]
  (if (not (seq? tree))
    (if (fn? tree) (:name (find-op tree funcs)) tree)
    (map (fn [t] (conv-code t funcs)) tree)))

(defn run-gp
  "Create a population of source trees and evolve them to fit the test function
   and data passed in."
  [{cycles :cycles gens :gens
    pop-size :pop-size forest-size :forest-size
    symbols :symbols funcs :funcs [term-max term-min] :const
    [depth-max depth-min] :depth [repfunc reprate] :rep
    mutation-rate :mutation tournament-size :tournament
    [testfit tests] :test}]

  ;;; The rest of the functions are wrapped in this closure so they have access
  ;;; to all the parameters passed to run-gp. This is much simpler than propagating
  ;;; all those parameters to the different functions. 

  (def actual
    "Find the \"actual\" or known test output by running all the training
     case inputs against the training function."
    (map (fn [x] (apply testfit x)) tests))

  (defn terminal
    "Return a random terminal for the source tree."
    [] (if (flip 0.5) (rand-nth symbols)
           (+ term-min (rand-int (- term-max term-min)))))

  ;;; **build-tree** is a combination of the "grow" and "fill" methods of tree building,
  ;;; similar to Koza's "ramped half-and-half" method.
  (defn build-tree
    "Build a random tree of lisp code. The first parameter of the function is the
     maximum depth of the tree, and the second is the minimum depth --- the tree
     will be filled up to the minimum depth, then grown to the maximum depth."
    [depth-max depth-min]
    (if (or (zero? depth-max)
            (and (<= depth-min 0) (flip 0.5)))
      (terminal) ; insert a random terminal
      (let [f (rand-nth funcs)]
        (cons (:op f) ; cons the operation onto a sublist matching the arity of f
              (repeatedly (:arity f) #(build-tree (- depth-max 1) (- depth-min 1)))))))

  (defn max-tree-height [tree]
    (if (not (seq? tree)) 0
        (+ 1 (reduce max (map max-tree-height tree)))))

  ;;; I used clojure's ability to define functions of multiple arities to make
  ;;; the rand-subtree and replace-subtree functions recurse based on height
  ;;; but not require height as a parameter.

  (defn rand-subtree
    "Return a random subtree of a list (presumably of lisp code)."
    ([tree] (rand-subtree tree (rand-int (max-tree-height tree))))
    ([tree n] (if (or (not (seq? tree)) (= n 0)) tree
                  (recur (rand-nth (rest tree)) (rand-int (- n 1))))))

  (defn replace-subtree
    "Replace a random subtree with a given subtree."
    ([tree sub] (replace-subtree tree sub (max-tree-height tree)))
    ([tree sub n] (if (or (not (seq? tree)) (= n 0)) sub
                      (let [r (+ 1 (rand-int (- (count (rest tree)) 1)))]                 
                        (concat (take r tree)
                                (list (replace-subtree
                                       (nth tree r) sub (rand-int (- n 1))))
                                (nthrest tree (+ r 1)))))))

  ;;; With rand-subtree and replace-subtree out of the way, the rest of the
  ;;; single-generation pass is pretty simple. Mutation and crossover both
  ;;; can easily be written in terms of rand-subtree and replace-subtree.

  (defn mutate
    "Mutate a tree by substituting in a randomly-built tree of code. The first
     parameter is the tree to potentially mutate, and the second parameter is
     the probability of mutation"
    [tree chance]
    (if (flip chance) (replace-subtree tree (build-tree 2 1)) tree))

  (defn crossover
    "The crossover function is simple to define in terms of replace-subtree
     and rand-subtree. Basically, crossing over two trees involves selecting a
     random subtree from one tree, and placing it randomly in the other tree."
    [tree1 tree2] (replace-subtree tree1 (rand-subtree tree2)))

  (defn build-forest
    "Returns a sequence of trees. A bunch of trees is a forest, right? Get it?"
    [size options] (repeatedly size #(apply build-tree options)))

  ;;; The selection process is convenient to express in lisp using heigher-order
  ;;; functions and **map**.

  (defn off-by [x y] (math/abs (- x y)))

  (defn find-error
    "Compares the output of the individual tree with the test data to calculate error."
    [tree]
    (let [func (eval (list 'fn symbols tree))]
      (reduce + (map off-by (map (fn [arg] (apply func arg)) tests) actual))))

  ;;; A few of the following functions refer to **ferror**, a sequence returned
  ;;; by forest-error. It's a sequence of maps, each with keys for :tree and
  ;;; :fitness. The keys correspond to the source tree and the fitness score,
  ;;; respectively. It's simple enough to sort the sequence by fitness, for
  ;;; example.
  
  (defn forest-error
    "Runs find-error on every tree in parallel, and returns a map with keys
     :tree and :fitness in place of each tree."
    [forest]
    (pmap (fn tree-error [tree] {:tree tree :fitness (find-error tree)}) forest))

  (defn tournament-select-error
    "Select out a few individuals (size specified by first parameter) and run a
     tournament amongst them. The two most fit in the tournament are crossed over
     to produce a child. Larger tournaments lead to more selective pressure."
    [tournament-size ferror]
    (let [selected (sort-by :fitness (repeatedly tournament-size #(rand-nth ferror)))]
      (crossover (:tree (first selected)) (:tree (second selected)))))

  (defn tournament-select
    "Run tournament-select-error enough times to re-populate the forest."
    [tournament-size ferror]
    (repeatedly (count ferror) #(tournament-select-error tournament-size ferror)))

  (defn get-best [ferror]
    (first (sort-by :fitness ferror)))

  (defn generations
    "Run n generations of a forest. The best individual seen so far in the forest is
     saved and passed on as the last parameter (it is nil when no best individual has
     been found)."
    [n forest best]
    (if (or (zero? n)
            (and (not (nil? best))
                 (zero? (:fitness best)))) ; Stop when fitness is zero.
      {:forest forest :best best}
      (do (when (mod n reprate)
            (repfunc best false))
          ; This let form establishes the "best" and "forest" parameters to
          ; use in the recursive call.
          (let [ferror (forest-error forest)
                cur-best (get-best ferror)
                new-best (if (nil? best) cur-best
                             (if (> (:fitness cur-best) (:fitness best)) best cur-best))
                new-forest (map (fn [tree] (mutate tree mutation-rate))
                                (tournament-select tournament-size ferror))]
          (recur (- n 1)
                 (if (nil? best) new-forest
                     (conj (rest new-forest)
                       (:tree new-best)))
                 new-best)))))
  
  (defn build-population
    "Call build-forest repeatedly to fill a population."
    [] (repeatedly pop-size #(build-forest forest-size [depth-max depth-min])))

  (defn population-crossover
    "Individual trees migrate between forests."
    [population]
    (let [cross (map rand-nth population)]
      (map (fn [[forest selected]] (conj (rest (shuffle forest)) selected))
           (zipmap population cross))))

  ;;; **parallel-generations** is the function that runs the show. It runs the
  ;;; generations function defined above on each of the forests (and does so in
  ;;; parallel, thanks to Clojure's convenient parallelism features).
  
  (defn parallel-generations
    "Spawn threads to run each of the forests for a specified amount of time, and
     cross over between the forests at specified intervals." 
    [cycles gens population best]
    (if (nil? population) (recur cycles gens (build-population) nil)
        (if (or (= cycles 0)
                (and (not (nil? best))
                     (zero? (:fitness best))))
          {:population population :best best}
          (do (when (and (not (nil? best)) (mod cycles reprate)) (repfunc best true))
              (let [p (pmap (fn [forest] (generations gens forest best)) population)
                    cur-pop (population-crossover (map :forest p))
                    all-bests (map :best p)
                    cur-best (first (sort-by :fitness all-bests))
                    new-best (if (nil? best) cur-best
                                 (if (> (:fitness cur-best) (:fitness best))
                                   best cur-best))]
                (recur (- cycles 1) gens cur-pop new-best))))))

  (parallel-generations cycles gens nil nil))
