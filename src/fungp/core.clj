;;; Mike Vollmer, 2012, GPL
;;;
;;; Downloading and Installing
;;; --------------------------
;;;
;;; [This project is hosted on GitHub.](https://github.com/probabilityZero/fungp)
;;;
;;; To install it, you can clone the git repository or grab the zip/tar file from
;;; the above link. You'll need Clojure installed, and you'll probably want
;;; [Leiningen](https://github.com/technomancy/leiningen) (the latter will take
;;; care of the former). See below for how to run the samples included with this
;;; library.
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
;;;  * Evolve subroutines
;;;  * Evolve code that interacts with Java or other JVM languages
;;;
;;; How do I use it?
;;; ----------------
;;;
;;; Call the **run-genetic-programming** function with a map containing these keyword parameters:
;;;
;;; * iterations : number of iterations *between migrations*
;;; * migrations : number of migrations
;;; * num-islands : number of islands
;;; * population-size : size of the populations
;;; * tournament-size : size of the tournaments (default 5)
;;; * mutation-probability : probability of mutation (default 0.2)
;;; * mutation-depth : depth of mutated trees (default 6)
;;; * max-depth : maximum depth of trees
;;; * terminals : terminals used in tree building
;;; * numbers : number literals to use as terminals (default [])
;;; * functions : functions used in tree building, in the form [function arity]
;;; * fitness : a fitness function that takes a tree and returns an error number, lower is better
;;; * report : a reporting function passed [best-tree best-fit] at each migration
;;; * adf-count : number of automatically defined functions (default 0)
;;; * adf-arity : number of arguments for automatically defined functions (default 1)
;;;
;;; Most fitness functions will *eval* the trees of code they are passed.
;;; In Clojure, eval'ing a list of code will compile it to JVM
;;; bytecode.
;;;
;;; If you're interested primarily in how to *use* fungp, you can skip to the example files.
;;;
;;; Notes on mutable variables and loops
;;; ------------------------------------
;;;
;;; *fungp* is fully capable of evolving code that uses mutable variables, side-effects, and
;;; loops. Currently it does not support Koza's architecture-altering operations so you have
;;; to determine some of the architecture beforehand (like, the names of the variables, whether
;;; you need a loop, etc).
;;;
;;; There also is no built-in mechanism for managing mutable variables --- I would like there
;;; to be, but there are a few obstacles. Clojure doesn't really support mutable local variables.
;;; That's certainly not a bad thing, but it makes it slightly more complicated to create
;;; local variables (or something that acts like them) at runtime. Because of this, I decided to leave
;;; the management of local variables up to the user, which (thanks to dynamic Vars) is fairly
;;; straightforard. See the compile-ants sample for an example of this.
;;;
;;; Environment, distribution
;;; -------------------------
;;;
;;; I highly recommend you use Leiningen. *fungp* is set up as a Leiningen project. 
;;; [Leiningen](https://github.com/technomancy/leiningen#installation) (or "lein") is available 
;;; for all major operating systems and is trivial to install.
;;;
;;; By itself *fungp* has no "main" method, so running it won't do anything. The samples can be
;;; run individually, and each has a "test-*" function that launches it. I recommend you run
;;; them from the REPL, as they have some settable parameters.
;;;
;;; Here's how you would run the cart example, starting from the fungp directory:
;;;
;;;     > lein repl
;;;     nREPL server started on port 54110
;;;     user=> (use 'fungp.sample.cart)
;;;     nil
;;;     user=> (test-cart 3 6)
;;;     ...
;;;
;;; At that point the cart example will run.
;;;
;;; The code
;;; --------
;;;
;;; The code here in the core.clj file is rather brief (but dense), and it should be readable
;;; in one sitting. To actually use it, you'll likely have to at least read the sample code, and
;;; probably read most of this code as well.
;;;

(ns fungp.core
  "This is the namespace declaration. It is the start of the core of the library."
  (:use fungp.util)
  (:use fungp.defined-branches))

;;; ### Tree creation
;;;
;;; My method of random tree generation is a combination of the "grow" and "fill"
;;; methods of tree building, similar to Koza's "ramped half-and-half" method.

(defn terminal
  "Return a random terminal or number."
  [terminals numbers]
  (if (and (flip 0.5)
           (not (empty? numbers)))
    (rand-nth numbers)
    (rand-nth terminals)))

(defn create-tree
  "Build a tree of source code, given a mutation depth, terminal sequence,
   function sequence, and type keyword. The type can be either :grow or :fill.
   The terminal sequence should consist of symbols or quoted code, while elements in the
   function sequence should contain both the function and a number representing
   its arity, in this form: [function arity]."
  [mutation-depth terminals numbers functions gtype]
  ;; conditions: either return terminal or create function and recurse
  (cond (zero? mutation-depth) (terminal terminals numbers)
        (and (= gtype :grow)
             (flip 0.5))
        (terminal terminals numbers)
        :else (let [[func arity] (rand-nth functions)]
                (cons func (repeatedly arity
                                       #(create-tree (dec mutation-depth)
                                                     terminals
                                                     numbers
                                                     functions
                                                     gtype))))))
;;; #### Branches
;;;
;;; This is a good time to introduce one of the defining features of the code evolved
;;; in fungp: it consists of multiple ordered branches, the last of which is the
;;; Result Defining Branch, or the branch that actually returns the final result.
;;; Other branches in the evolved code are stored in a let statement, which in
;;; Clojure binds and evaluates sequentially.
;;;
;;; For example:
;;;
;;;     (let [a 5
;;;           b (fn [x] (* a x))]
;;;       (b a))
;;;
;;; The above code would bind a to 5, and b to the anonymous function. The reference
;;; to a in b will resolve to 5, because elements in let are evaluated and bound in
;;; order. Finally, the expression (b a) would evaluate to 25.
;;;
;;; Most likely the type of branch you'll find most useful are Automatically Defined
;;; Functions. These are branches that are separate functions. They are exposed to
;;; the main branch, so the main branch can call the separately evolved functions,
;;; passing arguments to them.
;;;
;;; In addition to the ADFs, I implement a couple of other types of architecture
;;; described by Koza. I don't, however, implement architecture altering operations,
;;; so whatever architecture your individuals have must be determined beforehand.
;;;
;;; I also don't have support for automatically defined memory. See compiled-ants
;;; for an example of how to support mutable local variables (a feature that doesn't
;;; really exist in Clojure). For more information on the different types of branches
;;; in *fungp* see fungp.defined-branches.


(defn create-module-tree
  "This is a version of create-tree that handles multiple branches. It builds a let form that has a (potentially empty)
   vector for automatically defined functions and loops, and it has a main branch that is the result defining
   branch."
  [mutation-depth terminals numbers functions adf-arity adf-count adl-count adl-limit type]
  (list 'let
        ;; create the vector of branches
        (build-branch-vector create-tree mutation-depth terminals numbers functions
                             adf-arity adf-count adl-count adl-limit)
        ;; create the main branch
        (create-tree mutation-depth (concat terminals (gen-adl-terminals adl-count)) numbers
                     ;; add the branches to the function vector
                     (concat functions (gen-adf-func adf-count adf-arity))
                     type)))

;;; #### Populations
;;;
;;; A population is a list of individuals. Creating the population involves randomly generating a list of individuals
;;; as a starting point. Islands, which will be implemented further down, are lists of populations

(defn create-population
  "Creates a population of trees given a population size, mutation depth, terminal
   sequence, and function sequence. It uses a variation of Koza's \"ramped half-and-half\"
   method: a coin flip determines whether to use the \"fill\" or \"grow\" method, and the
   mutation depth is a randomly chosen number between 1 and the specified max mutation depth."
  [population-size mutation-depth terminals numbers functions adf-arity adf-count adl-count adl-limit]
  (if (zero? population-size) []
    (conj (create-population (dec population-size) mutation-depth terminals numbers functions
                             adf-arity adf-count adl-count adl-limit)
          (create-module-tree (inc (rand-int mutation-depth)) terminals numbers functions
                              adf-arity adf-count adl-count adl-limit
                              (if (flip 0.5) :grow :fill)))))

;;; ### Tree manipulation
;;;
;;; **rand-subtree** and **replace-subtree** are two of the most important functions.
;;; They define how the trees are modified.
;;;
;;; The basic idea for how I implemented both of them is that recursion can be
;;; used to reduce the problem at each step: given a tree (or a subtree, all of
;;; which have the same form), recurse on a random subtree, along with a
;;; reduced value of n. The base case is when n is zero or the function hits a leaf.
;;;
;;; Additionally, **replace-subtree** uses concat to reconstruct
;;; the tree on its way back up the stack.

(defn rand-subtree
  "Return a random subtree of a list. Takes an optional second parameter that limits
   the depth to go before selecting a crossover point."
  ([tree]
    (rand-subtree tree (rand-int (inc (max-tree-height tree)))))
  ([tree n]
    (if (or (zero? n) (and (seq? tree) (= (count tree) 1)) ;; don't split up (leaf)
                           (not (seq? tree))) tree
      (recur (rand-nth (rest tree))
             (rand-int n)))))

(defn replace-subtree
  "Replace a random subtree with a given subtree. Takes an optional second parameter
   that limits the depth to go before selecting a crossover point."
  ([tree sub]
    (replace-subtree tree sub (rand-int (+ 1 (max-tree-height tree)))))
  ([tree sub n]
    (if (or (zero? n) (and (seq? tree) (= (count tree) 1)) ;; don't split up (leaf)
                           (not (seq? tree))) sub
      (let [r (+ 1 (rand-int (count (rest tree))))]
        (concat (take r tree)
                (list (replace-subtree
                        (nth tree r) sub
                        (rand-int n)))
                (nthrest tree (inc r)))))))

(defn truncate
  "Prevent trees from growing too big by lifting a subtree if the tree height is
   greater than the max tree height."
  [tree height]
  (if (> (max-tree-height tree) height)
    (recur (rand-subtree tree) height)
    tree))

(defn truncate-module
  "A version of truncate that handles branches."
  [tree height]
  (list (first tree)
        (truncate-branch tree truncate height)
        (truncate (nth tree 2) height)))

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


(defn mutate-tree
  "Mutate a tree. Mutation takes one of three forms, chosen randomly: replace a random
   subtree with a newly generated tree, replace a random subtree with a terminal, or
   \"lift\" a random subtree to replace the root. The function takes a tree, mutation rate,
   a mutation depth (max size of new subtrees), terminals, and functions."
  [tree mutation-probability mutation-depth terminals numbers functions]
  (if (flip mutation-probability)
    (let [coin (rand)] ;; random number between 0 and 1
      (cond (< coin 0.33)
            (replace-subtree tree (create-tree mutation-depth terminals numbers functions :grow))
            (< coin 0.66)
            (replace-subtree tree (rand-nth terminals))
            :else (rand-subtree tree)))
    tree))

(defn mutate-module-tree
  "A version of mutate-tree that handles branches. It applies the mutation operation not only to the
   result defining branch but to the automatically defined branches in the let statement, and it preserve
   the overall structure of the tree (the let form)."
  [module-tree mutation-probability mutation-depth terminals numbers functions]
  (if (or (flip 0.5)
          (zero? (count (second module-tree))))
    ;; mutate the main branch
    (concat (take 2 module-tree)
            (list (mutate-tree (nth module-tree 2) mutation-probability
                               mutation-depth terminals numbers functions)))
    (concat (list (nth module-tree 0))
            (list (vec (map (fn [letf]
                         ;; mutate the branches
                         (mutate-branch letf mutate-tree
                                        mutation-probability mutation-depth
                                        terminals numbers functions))
                            (nth module-tree 1))))
            (list (nth module-tree 2)))))

(defn mutate-population
  "Apply mutation to every tree in a population. Similar arguments to mutate-tree."
  [population mutation-probability mutation-depth terminals numbers functions]
  (map #(mutate-module-tree % mutation-probability mutation-depth terminals
                            numbers functions) population))

;;; **Crossover** is the process of combining two parents to make a child.
;;; It involves copying the genetic material (in this case, lisp code) from
;;; the two parents, combining them, and returning the result of the combination.

(defn crossover
  "The crossover function is simple to define in terms of replace-subtree
   and rand-subtree. Basically, crossing over two trees involves selecting a
   random subtree from one tree, and placing it randomly in the other tree."
  [tree1 tree2] (replace-subtree tree1 (rand-subtree tree2)))

(defn crossover-module
  "Crossover that preserves the let form and branches."
  [tree1 tree2]
  (if (or (flip 0.5)
          (zero? (count (second tree1))))
    (let [new-subtree (crossover (nth tree1 2) (nth tree2 2))]
      (list (first tree1) (vec (second tree1)) new-subtree))
    (let [cross-branch (+ 1 (* 2 (rand-int (/ (count (second tree1))))))]
      (list (first tree1)
            (vec (crossover-branch cross-branch crossover
                                   (second tree1) (second tree2)))
            (nth tree1 2)))))

;;; Now it's time to get into functions that operate on populations.
;;;
;;; **Selection** is the process in which more fit individuals are "selected," or
;;; more likely to breed (be involved in a crossover), while less fit individuals
;;; are less likely to breed.
;;;
;;; To carry out the selection phase, it's necessary to determine how fit the
;;; individuals are. The following functions use the fitness function to give the
;;; individual trees a grade (I sometimes refer to it as "error"). Lower grades are
;;; better. Then, in the selection phase, individuals with lower error are more
;;; likely to be selected for crossover, and thus pass on their genetic
;;; material to the next generation.
;;;
;;; Note that the fitness function is expected to return *lower* numbers for better results,
;;; with 0 being "perfect." Execution will stop once 0 is reached.

(defn truncate-population
  "Apply truncate to all individuals in a population."
  [population height]
  (map #(truncate-module % height) population))

(defn fitness-zip
  "Compute the fitness of all the trees in the population, and map the trees to their population in a
   seq of a zipmap."
  [population fitness]
  (seq (zipmap population (map fitness population))))

(defn tournament-selection
  "Use tournament selection to create a new generation. In each tournament the two best individuals
   in the randomly chosen group will reproduce to form a child tree. A larger tournament size
   will lead to more selective pressure. The function takes a population, tournament size,
   \"fitness-zip\" or sequence of the zip of trees and fitness scores, and the max depth,
   and it returns a new population."
  [population tournament-size fitness-zip]
  (let [child
        (fn []
          (let [selected (map first (sort-by second
                                             (repeatedly tournament-size
                                                         #(rand-nth fitness-zip))))]
            (crossover-module (first selected)
                              (second selected))))]
    (repeatedly (count population) child)))

(defn get-best-fitness
  "Takes the fitness zip and finds the best in the population."
  [fitness-zip]
  (first (sort-by second fitness-zip)))

(defn elitism
  "Put the best-seen individual back in the population."
  [population best]
  (conj (rest population) best))

;;; ### Putting it together
;;;
;;; This takes care of all the steps necessary to complete one generation of the algorithm.
;;; The process can be extended to multiple generations with a simple tail recursive
;;; function.
;;;
;;; There are some extra considerations here. The function should:
;;;
;;;  * stop when a perfect individual has been found, meaning fitness is zero
;;;
;;;  * be resumable, meaning the search can halt, returning information, and that information
;;;    can be passed back in to start the search at the same place


(defn generations
  "Runs n generations of a population, and returns the population and the best tree in the form [population best-tree fitness].
   Takes a long list of parameters. This function is meant to be called by island-generations, which in turn is
   called by run-genetic-programming."
  [n population tournament-size mutation-probability mutation-depth max-depth terminals
   numbers functions fitness]
  (loop [n (int n) population population] ;; optimize inner loop
    (let [computed-fitness (fitness-zip population fitness)
          [best-tree best-fit] (get-best-fitness computed-fitness)]
      (if (or (zero? n) (zero? best-fit)) ;; terminating condition
        [population best-tree best-fit]   ;; return
        (recur (- n 1)                    ;; else recurse
               (-> population
                   (tournament-selection tournament-size computed-fitness)
                   (mutate-population mutation-probability mutation-depth terminals numbers functions)
                   (truncate-population max-depth)
                   (elitism best-tree)))))))

;;; ### Islands
;;;
;;; The above code works for running generations of a single population. The concept of islands is
;;; to have multiple separated populations evolving in parallel, and cross over between them.
;;;
;;; Thanks to clojure's parallelism features, the islands actually do run in parallel. To an extent,
;;; anyway: they're processed with a thread pool of reasonable size ("reasonable size" being the key
;;; phrase -- clojure decides based on your availabe resources).
;;;
;;; So, islands do two things.
;;;
;;;  * Better results: by separating individuals for part of their evolution, and recombining them
;;;    occasionally, we get (hopefully) more diversity.
;;;
;;;  * Better performance: by exploiting multiple CPU cores with native threads, the program can
;;;    process more individuals quickly.

(defn create-islands
  "Create a list of populations (islands)."
  [num-islands population-size mutation-depth terminals numbers functions adf-arity adf-count
   adl-count adl-limit]
  (repeatedly num-islands #(create-population population-size mutation-depth terminals numbers
                                              functions adf-arity adf-count adl-count adl-limit)))

(defn island-crossover
  "Individuals migrate between islands."
  [islands]
  (let [cross (map rand-nth islands)]
    (map (fn [[island selected]] (conj (rest (shuffle island)) selected))
         (zipmap islands cross))))

;;; And... *drum roll*

(defn island-generations
  "Run generations on all the islands and cross over between them. See the documentation for the generations function.
   Returns with the form [island best-tree best-fit]."
  [n1 n2 islands tournament-size mutation-probability mutation-depth max-depth terminals
   numbers functions fitness report]
  (loop [n (int n1) islands islands] ;; optimize inner loop
    (let [islands-fit (pmap #(generations n2 % tournament-size mutation-probability
                                          mutation-depth max-depth terminals numbers
                                          functions fitness)
                            (if (> (count islands) 1) (island-crossover islands) islands))
          islands (map first islands-fit)
          [_ best-tree best-fit] (first (sort-by #(nth % 2) islands-fit))]
      (if (or (zero? n) (zero? best-fit))
        [islands best-tree best-fit]
        (do (report best-tree best-fit)
            (recur (- n 1) islands))))))

;;; ### Wrap it up
;;;
;;; This is the final function, the one to be called from outside. It takes a key->value hash as a parameter, so the
;;; options can be provided in any order and some have default values. See the top of this file for a complete
;;; explanation of each of the keyword parameters.

(defn run-genetic-programming
  "This is the entry function. Call this with a map of the parameters to run the genetic programming algorithm."
  [{:keys [iterations migrations num-islands population-size tournament-size mutation-probability
           mutation-depth max-depth terminals functions numbers fitness report adf-arity adf-count
           adl-count adl-limit]
    ;; the :or block here specifies default values for some of the arguments
   :or {tournament-size 3 mutation-probability 0.1 mutation-depth 6 adf-arity 1 adf-count 0
        adl-count 0 adl-limit 25 numbers []}}]
  ;; some basic type checking: most of the parameters must be integers
  (map #(assert (integer? %)) [iterations migrations num-islands population-size tournament-size mutation-probability
                               mutation-depth max-depth adf-arity adf-count adl-count adl-limit])
  ;; report and fitness should be functions
  (map #(assert (fn? %)) [report fitness])
  ;; call island generations
  (island-generations migrations iterations
                      (create-islands num-islands population-size mutation-depth terminals
                                      numbers functions adf-arity adf-count adl-count adl-limit)
                      tournament-size mutation-probability mutation-depth max-depth terminals
                      numbers functions fitness report))

;;; And that's it! For the core of the library, anyway.
