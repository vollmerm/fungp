;;; Mike Vollmer, 2012, GPL
;;;
;;; [Project hosted on GitHub](https://github.com/probabilityZero/fungp)
;;;
;;; What is this?
;;; -------------
;;;
;;; **fungp** is a parallel genetic programming library implemented in the
;;; Clojure programming language, pronounced fun-gee-pee. The "fun" comes
;;; from functional, and because genetic programming can be fun! Also I'm
;;; bad at naming things.
;;;
;;; > There are only two hard things in Computer Science: cache invalidation,
;;; > naming things, and off-by-one errors.
;;; >
;;; > --- *Paraphrased from Phil Karlton*
;;; 
;;; The library is in its early stages right now, but it's usable. Currently it
;;; has the following features:
;;;
;;;  * Custom evaluation and reporting logic
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
;;;     (run-gp {:gens iter :cycles cycle
;;;             :pop-size 6 :forest-size 50
;;;             :symbols symbols :funcs funcs
;;;             :term-max 1 :term-min -1
;;;             :max-depth 4 :min-depth 2
;;;             :repfunc repfunc  :reprate 1
;;;             :mutation-rate 0.1 :tournament-size 5
;;;             :actual actual :tests testdata})
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
;;; For more information on how to use it, see the source code below. The
;;; code is sometimes dense (it's amazing how a few lines of lisp code can
;;; do as much or more as a hundred lines of a more verbose language like
;;; C or Java), but it shouldn't be too hard to understand the general
;;; concepts, especially if you have some familiarity with lisp.

(ns fungp.core
  "This is the start of the core of the library."
  (:use fungp.util))

;;; ### Options
;;;
;;; Before I get into the implementation of the algorithm, I need to get this
;;; out of the way. The options hash has all the information necessary to
;;; run the rest of the functions, and for sake of simplicity I just lump it
;;; all together and pass it to any function that needs it. This function takes
;;; the user's input and merges it with a hash of defaults (the user's options
;;; will override the defaults, but the defaults will be used if the user does
;;; not specify that option).
;;;
;;;The following options keywords are accepted:
;;;
;;; * *pop-size* --- the number of forests, and the number of top-level threads to run
;;; * *forest-size* --- the number of trees in each forest
;;; * *symbols* --- a sequence of symbols to be placed in the generated code as terminals
;;; * *funcs* --- a sequence (following a certain format; see core.clj or sample.clj) describing the functions to be used in the generated code
;;; * *term-max* and *term-min* --- the range of number terminals to be used in generated code (default to 1 and -1, respectively)
;;; * *depth-max* and *depth-min* --- the minimum/maximum height of randomly generated trees (defaults to 2 and 1, respectively)
;;; * *repfunc* --- the reporting function, which gets passed the best-seen individual (a hash with keys :tree and :fitness; see sample.clj for an example)
;;; * *reprate* --- the reporting rate; every nth cycle repfunc will be called
;;; * *mutation-rate* --- a number between 0 and 1 that determines the chance of mutation (defaults to 0.05)
;;; * *tournament-size* --- the number of individuals in each tournament selection (defaults to 5)
;;; * *tests* --- test inputs for your function, in the form of a sequence of vectors (each should match the length of *symbols* above)
;;; * *actual* --- the correct outputs for each of the *tests* elements

(defn build-options
  "Take passed-in parameters and merge them with default parameters to construct
   the options hash that gets passed to the other functions."
  [o] (let [defaults {:term-max 1 :term-min -1 :depth-max 2 :depth-min 1
                      :mutation-rate 0.05 :tournament-size 5}]
        (merge defaults o)))

;;; Many of the functions below use the options hash built by the build-options
;;; function. In most cases this is invisible, since run-gp constructs the
;;; options hash and runs the parallel-generations function, but for purposes
;;; of testing or modification/extension you can do this manually with build-options.

(defn terminal
  "Return a random terminal for the source tree. Takes the options hash as parameter."
  [o] (if (flip 0.5) (rand-nth (:symbols o))
          (+ (:term-min o) (rand-int (- (:term-max o) (:term-min o))))))

;;; ### Tree manipulation
;;;
;;; My method of random tree generation is a combination of the "grow" and "fill"
;;; methods of tree building, similar to Koza's "ramped half-and-half" method.

(defn build-tree
  "Build a random tree of lisp code. The tree will be filled up to the minimum depth,
   then grown to the maximum depth. Minimum and maximum depth are specified in the
   options, but can optionally be passed explicitly."
  ([o] (build-tree o (:depth-max o) (:depth-min o)))
  ([o depth-max depth-min]
     (if (or (zero? depth-max)
             (and (<= depth-min 0) (flip 0.5)))
       (terminal o) ;; insert a random terminal
       (let [f (rand-nth (:funcs o))]
         (cons (:op f) ;; cons the operation onto a sublist matching the arity of f
               (repeatedly (:arity f) #(build-tree o (- depth-max 1) (- depth-min 1))))))))

;;; First we define a function for creating a collection of trees, then one for
;;; creating a collection of a collection of trees.

(defn build-forest
  "Returns a sequence of trees. A bunch of trees is a forest, right? Get it?"
  [o] (repeatedly (:forest-size o) #(build-tree o)))

(defn build-population
  "Call build-forest repeatedly to fill a population. A population is a collection
   of forests."
  [o] (repeatedly (:pop-size o) #(build-forest o)))

(defn max-tree-height
  "Find the maximum height of a tree."
  [tree] (if (not (seq? tree)) 0 (+ 1 (reduce max (map max-tree-height tree)))))

;;; **rand-subtree** and **replace-subtree** are two of the most important functions.
;;; They define how the trees are modified.

;;; The basic idea for how I implemented both of them is that recursion can be
;;; used to reduce the problem at each step: given a tree (or a subtree, all of
;;; which have the same form), recurse on a random subtree, along with a
;;; reduced value of n. The base case is when n is zero or the function hits a leaf.
;;;
;;; Additionally, **replace-subtree** uses concat to reconstruct
;;; the tree on its way back up the stack.

(defn rand-subtree
  "Return a random subtree of a list."
  ([tree] (rand-subtree tree (rand-int (+ 1 (max-tree-height tree)))))
  ([tree n] (if (or (zero? n) (not (seq? tree))) tree
                (recur (rand-nth (rest tree))
                       (rand-int n)))))

(defn replace-subtree
  "Replace a random subtree with a given subtree."
  ([tree sub] (replace-subtree tree sub (rand-int (+ 1 (max-tree-height tree)))))
  ([tree sub n] (if (or (zero? n) (not (seq? tree))) sub
                    (let [r (+ 1 (rand-int (count (rest tree))))] 
                      (concat (take r tree)
                              (list (replace-subtree
                                     (nth tree r) sub
                                     (rand-int n)))
                              (nthrest tree (+ r 1)))))))

;;; ### Mutation, crossover, and selection
;;;
;;; With rand-subtree and replace-subtree out of the way, the rest of the
;;; single-generation pass is pretty simple. Mutation and crossover both
;;; can easily be written in terms of rand-subtree and replace-subtree.
;;;
;;; **Mutation** takes a tree and (occasionally) randomly changes part of it.
;;; The idea, like the rest of the fundamental aspects of genetic algorithms,
;;; is taken from nature; when DNA is copied, there is a slight chance of
;;; "mistakes" being introduced in the copy. This can lead to beneficial
;;; changes and increases genetic diversity.

(defn mutate
  "Mutate a tree by substituting in a randomly-built tree of code."
  [o tree] (if (flip (:mutation-rate o))
             (replace-subtree tree (build-tree o)) tree))

;;; **Crossover** is the process of combining two parents to make a child.
;;; It involves copying the genetic material (in this case, lisp code) from
;;; the two parents, combining them, and returning the result of the combination.

(defn crossover
  "The crossover function is simple to define in terms of replace-subtree
   and rand-subtree. Basically, crossing over two trees involves selecting a
   random subtree from one tree, and placing it randomly in the other tree."
  [tree1 tree2] (replace-subtree tree1 (rand-subtree tree2)))

;;; **Selection** is the process in which more fit individuals are "selected," or
;;; more likely to breed (be involved in a crossover), while less fit individuals
;;; are less likely to breed.
;;;
;;; To carry out the selection phase, it's necessary to determine how fit the
;;; individuals are. The following functions use the training data to give the
;;; individual trees a grade, which is the sum of the error. Lower grades are
;;; better. Then, in the selection phase, individuals with lower error are more
;;; likely to be selected for crossover, and thus pass on their genetic
;;; material to the next generation.

(defn find-error
  "Compares the output of the individual tree with the test data to calculate error."
  [o tree]
  (let [func (eval (list 'fn (:symbols o) tree))]
    (reduce + (map off-by (map (fn [arg] (apply func arg)) (:tests o)) (:actual o)))))

(defn forest-error
  "Runs find-error on every tree in parallel, and returns a map with keys
   :tree and :fitness in place of each tree. It needs the options hash because
   find-error needs to extract symbols, tests and actual."
  [o forest]
  (pmap (fn tree-error [tree] {:tree tree :fitness (find-error o tree)}) forest))

;;; A few of the following functions refer to **ferror**, a sequence returned
;;; by forest-error. It's a sequence of maps, each with keys for :tree and
;;; :fitness. The keys correspond to the source tree and the fitness score,
;;; respectively. It's simple enough to sort the sequence by fitness, for
;;; example.

(defn get-best
  "Returns the best tree, given ferror."
  [ferror] (first (sort-by :fitness ferror)))

(defn tournament-select-error
  "Select out a few individuals (tournament size is in o) and run a
   tournament amongst them. The two most fit in the tournament are crossed over
   to produce a child. Larger tournaments lead to more selective pressure."
  [o ferror]
  (let [selected (sort-by :fitness (repeatedly (:tournament-size o) #(rand-nth ferror)))]
    (crossover (:tree (first selected)) (:tree (second selected)))))

(defn tournament-select
  "Run tournament-select-error enough times to re-populate the forest. The options
   hash is passed so tournament-select-error can extract tournament-size."
  [o ferror] (repeatedly (count ferror) #(tournament-select-error o ferror)))

;;; ### Putting it together
;;;
;;; This takes care of all the steps necessary to complete one generation of the algorithm.
;;; The process can be extended to multiple generations with a simple tail recursive
;;; function.
;;;
;;; There are some extra considerations here. The function should:
;;;
;;;  * stop when a perfect individual has been found, meaning fitness is zero
;;;  * be resumable, meaning the search can halt, returning information, and that information
;;;    can be passed back in to start the search at the same place

(defn generations
  "Run n generations of a forest. Over the course of one generation, the trees in
   the forest will go through selection, crossover, and mutation. The best individual
   seen so far in the forest is saved and passed on as the last parameter (it is nil
   when no best individual has been found)."
  [o n forest best]
  (if (or (zero? n)
          (and (not (nil? best))
               (zero? (:fitness best)))) ;; stop early when fitness is zero
    {:forest forest :best best} ;; return forest and current best
    (let [ferror (forest-error o forest)
          cur-best (get-best ferror)
          new-best (if (nil? best) cur-best
                       (if (> (:fitness cur-best) (:fitness best)) best cur-best))
          new-forest (map (fn [tree] (mutate o tree))
                          (tournament-select o ferror))]
      ;; the recursive call for the next generation
      (recur o
             (- n 1)
             (if (nil? best) new-forest
                 (conj (rest new-forest)
                       (:tree new-best)))
             new-best))))

;;; ### Populations
;;;
;;; After building a single tree, then a single generation, then multiple generations,
;;; it's one more step to parallel generations. Above there's a function for defining
;;; a "population" of forests. We can evolve the forests in the population individually
;;; and cross over between them.

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
   cross over between the forests at specified intervals. If the search is starting
   from the beginning, the only necessary parameter is the options hash. The initial
   values of the other parameters can be inferred from it. If you're resuming a search
   you can simply pass in the population explicitly and this function will start
   where it left off."
  ([o] (parallel-generations o (:cycles o) (:gens o) (build-population o) nil))
  ([o cycles gens population best]
     (if (or (zero? cycles) (and (not (nil? best)) (zero? (:fitness best))))
       {:population population :best best} ;; done
       (do (when (and (not (nil? best)) (zero? (mod cycles (:reprate o))))
             ((:repfunc o) best)) ;; report
           ;; similar pattern to the generations function
           (let [p (pmap (fn [forest] (generations o gens forest best)) population)
                 cur-pop (population-crossover (map :forest p))
                 all-bests (map :best p)
                 cur-best (first (sort-by :fitness all-bests))
                 new-best (if (nil? best) cur-best
                              (if (> (:fitness cur-best) (:fitness best))
                                best cur-best))]
             (recur o (- cycles 1) gens cur-pop new-best))))))


(defn run-gp
  "Create a population of source trees and evolve them to fit the test function
   and data passed in. This is probably the function you'll want to call."
  [o] (parallel-generations (build-options o)))

;;; And that's it! For the core of the library, anyway. 
