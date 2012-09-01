;;; Sample: Cart problem
;;; --------------------
;;;
;;; This is an example of evolving a function that takes parameters and
;;; returns a number. In this case, the function acts as a controller for
;;; and imaginary cart.
;;;
;;; The cart is on a track and has a rocket attached to it. The goal of the
;;; controller is to aim the rocket either forward and backward and eventually
;;; center the cart. The controller function takes two parameters: the current
;;; speed and current direction. It returns either a positive or negative
;;; number to determine which direction to face. It applies a constant force
;;; in whichever direction it is facing.


(ns fungp.sample.cart
  (:use fungp.core)
  (:use fungp.util)
  (:require clojure.pprint))

;;; some constants

(def MAX_T "Max time limit" 5)
(def TEST_POINTS "Number of test cases" 20)

(defn move-cart
  "Simulate cart movement with the evolved function used to calculate
   thrust. Assuming acceleration is one unit/second^2, and time is in
   steps of 0.02."
  [t v x f]
  (loop [t (float t) v (float v) x (float x)]
    (cond (and (<= (Math/abs x) 0.01) (<= (Math/abs v) 0.01)) t
          (>= t MAX_T) 100
          :else
          (let [thrust (if (> (f v x) 0) 1.0 -1.0)]
            (recur (+ t 0.02)
                   (+ v (* thrust 0.02))
                   (+ x (* v 0.02)))))))

(defn rand-spread
  "Range of starting position or velocity"
  [l] (repeatedly l #(- (* 1.5 (rand)) 0.75)))

(def cart-functions "Functions available to the evolved programs"
  '[[+ 2][* 2][- 2][fungp.util/abs 1][fungp.util/gt 2]])

(def cart-terminals "Terminals available to the evolved programs"
  '[x v -1])

(def rand-velocity (rand-spread TEST_POINTS))
(def rand-position (rand-spread TEST_POINTS))

(defn cart-error
  "Calculate error using move-cart"
  [f]
  (reduce + (map (fn [v x] (move-cart 0 v x f))
                 rand-velocity
                 rand-position)))

;;; some existing solutions

(defn cart-optimal "The optimal solution" [v x] (gt (* -1 x) (* v (abs v))))

(defn cart-suboptimal [v x] (gt (* -1 x) (* v v)))

(def cart-optimal-error (cart-error cart-optimal))

(def cart-evol (fn [v x] (let [] (fungp.util/gt (- x v) (+ (+ x x) (+ x x))))))
(def cart-evol2 (fn [v x] (let [] (- (- (- -1 x) (+ x v))
                                     (- (* (+ -1 v) (* -1 x))
                                        (fungp.util/gt (fungp.util/abs x) -1))))))

(defn cart-fitness
  "Compute fitness for cart problem."
  [tree] (let [f (eval (list 'fn '[v x] tree))]
           (cart-error f)))

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
  (println "Error is number of seconds taken total across all the tests.")
  (println "If a program times out it is assigned 100 seconds.")
  (let [options {:iterations n1 :migrations n2 :num-islands 4
                 :population-size 100 :terminals cart-terminals
                 :tournament-size 3
                 :functions cart-functions :fitness cart-fitness
                 :report cart-report
                 :adf-count 1
                 :max-depth 5}
        [tree score] (rest (run-genetic-programming options))]
    (do (println "Done!")
        (cart-report tree score))))
