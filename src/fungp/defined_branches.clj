;;; Branches and Modular Structure
;;; ------------------------------
;;;
;;; *fungp* is capable of evolving multiple branches in an individual. In addition to the main branch,
;;; or the "Result Defining Branch," it can evolve functions (Automatically Defined Functions, or ADFs).
;;;
;;; Some of the handling for these branches was covered in fungp.core, but the details are handled here.

(ns fungp.defined-branches)

;;; ### Automatically Defined Functions
;;;
;;; ADFs are functions of arbitrary arity that can be called from the main branch. The following functions
;;; are helper functions for creating individuals with ADF branches.

(defn gen-adf-arg
  "Generate arguments vector for ADF"
  [adf-arity]
  (vec (map #(symbol (str "arg" %))
            (range adf-arity))))

(defn gen-adf-func
  "Generate ADF function names"
  [adf-count adf-arity]
  (when (> adf-arity 0)
    (vec (map (fn [n] [(symbol (str "adf" n))
                       adf-arity])
              (range adf-count)))))

(defn make-adf
  "Build the ADF code"
  [create-tree mutation-depth terminals numbers functions adf-arity num]
  (let [adf-args (gen-adf-arg adf-arity)]
  [(symbol (str "adf" num))
   (list 'fn adf-args
         (create-tree mutation-depth
                      (concat terminals adf-args)
                      numbers
                      functions
                      :grow))]))

(defn make-adf-branch
  "Make the ADF branch"
  [create-tree mutation-depth terminals numbers functions adf-arity adf-count]
  (if (zero? adf-count) []
      (concat (make-adf-branch create-tree mutation-depth terminals numbers
                               functions adf-arity (dec adf-count))
              (make-adf create-tree mutation-depth terminals numbers
                        (concat functions
                                (gen-adf-func (dec adf-count)
                                              adf-arity))
                        adf-arity (dec adf-count)))))



;;; ### Automatically Defined Loops
;;;
;;; ADLs are loops that evolve the different stages of the loop as distinct branches. The following
;;; functions are helper functions for creating individuals with ADL branches.
;;;
;;; Currently, ADLs are only partially implemented and not tested or stable.

(defn make-adl
  "Build the ADL code"
  [create-tree mutation-depth terminals numbers functions adl-limit num]
  (let [make-branch #(create-tree mutation-depth
                                  terminals
                                  numbers
                                  functions
                                  :grow)]
    [(symbol (str "adl" num))
     (list 'fungp.util/do-loop
           ;; do-loop has 4 branches of code and one number literal
           (make-branch)
           (make-branch)
           (make-branch)
           (make-branch)
           adl-limit)]))

(defn make-adl-branch
  "Make the ADL branch"
  [create-tree mutation-depth terminals numbers functions adl-count adl-limit]
  (if (zero? adl-count) []
    (concat (make-adl-branch create-tree mutation-depth terminals numbers
                             functions (dec adl-count) adl-limit)
            (make-adl create-tree mutation-depth terminals numbers
                      functions ; TODO: include ADFs and previous ADLs, if any
                      adl-limit (dec adl-count)))))

(defn gen-adl-terminals
  "Generate a vector of symbols to reference the result of the ADLs."
  [adl-count]
  (vec (map #(symbol (str "adl" %)) (range adl-count))))

;;; ### Putting it together

(defn build-branch-vector
  "This function builds the vector that goes into the let statement for each individual. It is intended to
   be called by create-module-tree."
  [create-tree mutation-depth terminals numbers functions adf-arity adf-count adl-count adl-limit]
  (vec (concat (if (zero? adf-count) '()
                   (make-adf-branch create-tree mutation-depth terminals numbers
                                    functions adf-arity adf-count))
               (if (zero? adl-count) '()
                   (make-adl-branch create-tree mutation-depth terminals numbers
                                    functions adl-count adl-limit)))))

(defn mutate-branch
  "Takes an element of the branch vector, a mutation function, and the tunable parameters
   the mutation function needs, and applies mutation to each of the branches."
  [letf mutate-tree mutation-probability mutation-depth terminals numbers functions]
  (cond (and (list? letf)
             (= 'fn (first letf)))
        (list (first letf) (second letf)
              ;; call mutate-tree on the ADF body
              (mutate-tree (nth letf 2)
                           mutation-probability
                           mutation-depth terminals
                           numbers functions))
        (and (list? letf)
             (= 'fungp.util/do-loop (first letf)))
        ;; it's a loop! mutate each branch individually
        (list (first letf)
              (mutate-tree (nth letf 1)
                           mutation-probability
                           mutation-depth terminals
                           numbers functions)
              (mutate-tree (nth letf 2)
                           mutation-probability
                           mutation-depth terminals
                           numbers functions)
              (mutate-tree (nth letf 3)
                           mutation-probability
                           mutation-depth terminals
                           numbers functions)
              (mutate-tree (nth letf 4)
                           mutation-probability
                           mutation-depth terminals
                           numbers functions)
              (nth letf 5))
        :else letf))

(defn truncate-branch
  "Call truncate on branches. Takes a tree, truncate function, and a height."
  [tree truncate height]
  (vec (map
        #(cond (and (list? %)
                    (= 'fn (first %)))
               (list (first %) (second %)
                     (truncate (nth % 2) height))
               (and (list? %)
                    (= 'fungp.util/do-loop (first %)))
               (list (first %)
                     (truncate (nth % 1) height)
                     (truncate (nth % 2) height)
                     (truncate (nth % 3) height)
                     (truncate (nth % 4) height)
                     (nth % 5))
               :else %)
        (second tree))))

(defn crossover-branch
  "Crossover function for the vectors of branches. Takes the cross point, the crossover function,
   and the two vectors."
  [cross crossover vec1 vec2]
  (let [tree1 (nth vec2 cross)
        tree2 (nth vec1 cross)
        newtree (cond (= (first tree1) 'fn)
                      (list (first tree1) (second tree1)
                            (crossover (nth tree1 2) (nth tree2 2)))
                      (= (first tree1) 'fungp.util/do-loop)
                      (list (nth tree1 0)
                            (crossover (nth tree1 1) (nth tree2 1))
                            (crossover (nth tree1 2) (nth tree2 2))
                            (crossover (nth tree1 3) (nth tree2 2))
                            (crossover (nth tree1 4) (nth tree2 4))
                            (nth tree1 5)))]
    (concat (take cross vec1)
            (list newtree)
            (drop (+ cross 1) vec1))))
