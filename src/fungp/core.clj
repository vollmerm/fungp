(comment
  [:title (fungp [functional genetic programming in clojure])
   :author (Mike Vollmer)
   :license GPL])

(ns fungp.core)

(require '[clojure.math.numeric-tower :as math])

(def funcs [{:op * :arity 2 :name '*}
            {:op + :arity 2 :name '+} 
            {:op - :arity 2 :name '-} 
            {:op inc :arity 1 :name 'inc} 
            {:op dec :arity 1 :name 'dec}])

(def symbols ['a])

(def symbtest '(fn [a] (+ -1 (* a (- a 1)))))

(def testfit (eval symbtest))

(def rtests (range -100 100))

(def testdata (map vector rtests))

(defn find-op [op funcs] 
  (first (filter (fn [x] (= (:op x) op)) funcs)))

(defn conv-code [tree funcs]
  (if (not (seq? tree))
    (if (fn? tree) (:name (find-op tree funcs)) tree)
    (map (fn [t] (conv-code t funcs)) tree)))

(defn repfunc [{tree :tree fitness :fitness} par]
  (when par
    (do
      (print "Code:\t")(print (conv-code tree funcs))(print "\n")
      (print "Error:\t")(print fitness)(print "\n\n"))))

(defn flip [chance]
  (< (rand) chance))

(defn run-gp [{cycles :cycles gens :gens
               pop-size :pop-size forest-size :forest-size
               symbols :symbols funcs :funcs [term-max term-min] :const
               [depth-max depth-min] :depth [repfunc reprate] :rep
               mutation-rate :mutation tournament-size :tournament
               [testfit tests] :test}]

  (def actual (map (fn [x] (apply testfit x)) tests))

  (defn terminal []
    (if (flip 0.5) (rand-nth symbols)
        (+ term-min (rand-int (- term-max term-min)))))

  (defn build-tree [depth-max depth-min]
    (if (or (zero? depth-max)
            (and (<= depth-min 0) (flip 0.5)))
      (terminal)
      (let [f (rand-nth funcs)]
        (cons (:op f)
              (repeatedly (:arity f) #(build-tree (- depth-max 1) (- depth-min 1)))))))

  (defn max-tree-height [tree]
    (if (not (seq? tree)) 0
        (+ 1 (reduce max (map max-tree-height tree)))))

  (defn rand-subtree
    ([tree] (rand-subtree tree (rand-int (max-tree-height tree))))
    ([tree n] (if (or (not (seq? tree)) (= n 0)) tree
                  (recur (rand-nth (rest tree)) (rand-int (- n 1))))))

  (defn replace-subtree
    ([tree sub] (replace-subtree tree sub (max-tree-height tree)))
    ([tree sub n] (if (or (not (seq? tree)) (= n 0)) sub
                      (let [r (+ 1 (rand-int (- (count (rest tree)) 1)))]                 
                        (concat (take r tree)
                                (list (replace-subtree
                                       (nth tree r) sub (rand-int (- n 1))))
                                (nthrest tree (+ r 1)))))))

  (defn mutate [tree chance]
    (if (flip chance)
      (replace-subtree tree (build-tree 2 1))
      tree))

  (defn crossover [tree1 tree2]
    (replace-subtree tree1 (rand-subtree tree2)))

  (defn build-forest [size options]
    (repeatedly size #(apply build-tree options)))

  (defn off-by [x y] (math/abs (- x y)))

  (defn find-error [tree]
    (let [func (eval (list 'fn symbols tree))]
      (reduce + (map off-by (map (fn [arg] (apply func arg)) tests) actual))))

  (defn forest-error [forest]
    (map (fn tree-error [tree]
           {:tree tree :fitness (find-error tree)})
         forest))

  (defn tournament-select-error [tournament-size ferror]
    (let [selected (sort-by :fitness (repeatedly tournament-size
                                                 #(rand-nth ferror)))]
      (crossover (:tree (first selected)) (:tree (second selected)))))

  (defn tournament-select [tournament-size ferror]
    (repeatedly (count ferror) #(tournament-select-error tournament-size ferror)))

  (defn get-best [ferror]
    (first (sort-by :fitness ferror)))

  (defn generations [n forest best]
    (if (or (= n 0)
            (and (not (nil? best))
                 (= (:fitness best) 0)))
      {:forest forest :best best}
      (do (when (mod n reprate)
            (repfunc best false))
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
  
  (defn build-population []
    (repeatedly pop-size #(build-forest forest-size [depth-max depth-min])))

  (defn population-crossover [population]
    (let [cross (map rand-nth population)]
      (map (fn [[forest selected]] (conj (rest (shuffle forest)) selected))
           (zipmap population cross))))

  (defn parallel-generations [cycles gens population best]
    (if (nil? population) (recur cycles gens (build-population) nil)
        (if (or (= cycles 0)
                (and (not (nil? best))
                     (= (:fitness best) 0)))
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

 (defn test-gp [cycle iter]
  (println "fungp :: Functional Genetic Programming in Clojure")
  (println "Mike Vollmer, 2012")
  (println "==================================================\n")
  (println "Attempting to minimize this function:")
  (println symbtest)
  (println "\nLower numbers are better. Results shown are sum of error.\n")
  (def results (run-gp {:gens iter :cycles cycle
                        :pop-size 6 :forest-size 50
                        :symbols symbols :funcs funcs
                        :const [1 -1] :depth [2 1]
                        :rep [repfunc 1] :mutation 0.1 :tournament 3
                        :test [testfit testdata]}))
  (def best-result (:best results))
  (def out-func (list 'fn symbols (conv-code (:tree best-result) funcs)))
  (println "Done!")
  (println out-func)
  (print "Lowest error: ")(print (:fitness best-result))(print "\n")
  (eval out-func))
   
