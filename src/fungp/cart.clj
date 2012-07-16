;;; Sample: Cart problem
;;; --------------------


(ns fungp.cart
  "Cart problem example from Koza"
  (:use fungp.core)
  (:use fungp.util)
  (:require clojure.pprint))

;;; some constants

(def MAX_T 5)
(def TEST_POINTS 20)

(defn one-or-neg-one
  "Wrap the output of the evolved function: positive values are +1 and
   negative values are -1"
  [x] (if (> x 0) 1 -1))

(defn move-cart
  "Simulate cart movement with the evolved function used to calculate
   thrust. Assuming acceleration is one unit/second^2, and time is in
   steps of 0.02."
  [t v x f]
  (if (or (<= (Math/abs x) 0.005)
          (>= t MAX_T))
    t ;; finished
    (let [thrust (one-or-neg-one (f v x))
          velocity (+ v (* thrust 0.02))
          position (+ x (* velocity 0.02))
          time (+ t 0.02)]
      (recur time velocity position f))))

(defn rand-spread
  "Range of starting position or velocity"
  [l] (repeatedly l #(- (* 1.5 (rand)) 0.75)))

(def cart-functions '[[+ 2][* 2][- 2][fungp.util/sdiv 2]
                      [fungp.util/abs 1][fungp.util/gt 2]])
(def cart-terminals '[x v -1])

(def rand-velocity (rand-spread TEST_POINTS))
(def rand-position (rand-spread TEST_POINTS))

(defn cart-error
  "Calculate error using move-cart"
  [f]
  (reduce + (map (fn [v x] (move-cart 0 v x f))
                 rand-velocity
                 rand-position)))

;;; some existing solutions

(defn cart-optimal [v x] (gt (* -1 x) (* v (abs v))))

(defn cart-suboptimal [v x] (gt (* -1 x) (* v v)))

(defn cart-evol [v x] (- (* -1 x) (+ x v)))

(def cart-optimal-error (cart-error cart-optimal))

(defn cart-fitness
  "Compute fitness for cart problem."
  [tree] (let [f (eval (list 'fn '[v x] tree))
               error (cart-error f)]
           (/ error TEST_POINTS)))

(defn cart-report
  "Reporting function. Prints out the tree and its score"
  [tree fitness]
  (clojure.pprint/pprint (list 'fn '[v x] tree))(print "\n")
  (print "Error:\t")(print fitness)(print "\n\n")
  (flush))

(defn test-cart
  "Run the cart problem"
  [n1 n2]
  (println "Koza's Cart Problem")
  (let [options {:iterations n1 :migrations n2 :num-islands 4
                 :population-size 25 :terminals cart-terminals
                 :tournament-size 3
                 :functions cart-functions :fitness cart-fitness
                 :report cart-report :adf-count 0
                 :max-depth 15}
        [tree score] (rest (run-genetic-programming options))]
    (do (println "Done!")
        (cart-report tree score)
        (eval (list 'fn '[v x] tree)))))
