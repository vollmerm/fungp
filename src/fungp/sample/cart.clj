;;; ### Cart problem
;;;
;;; This is an example of evolving a function that takes parameters and
;;; returns a number. In this case, the function acts as a controller for
;;; and imaginary cart.
;;;
;;; The cart is on a track and has a rocket attached to it. The goal of the
;;; controller is to aim the rocket either forward and backward and eventually
;;; center the cart. The controller function takes two parameters: the current 
;;; velocity, v, and the current position, x. When v is positive the cart is 
;;; moving to the right, and when v is negative the cart is moving to the left.
;;; The evolved function will use these inputs to return either a positive or 
;;; negative number, which designates which direction to point the rocket; 
;;; negative points it to the left, and positive points it to the right. 
;;; The rocket applies a constant force (1 unit/second^2) in whichever 
;;; direction it is facing on every tick of the clock.
;;; 
;;; Evolved programs will be tested with a variety of random starting 
;;; configurations (position and velocity) to see if the cart can be centered
;;; every time. 
;;; 
;;; To test an evolved program, we need to simulate a cart and let the 
;;; controller programs experiment by pushing it around. As they do this 
;;; they should "learn" how to center the cart. The simulation will work
;;; like this: the cart is placed in a random location with a random
;;; velocity, and, in a loop, the simulation function will call the
;;; evolved function with the state of the cart as input, and use the
;;; output of the program to update the state of the cart. Remember, the
;;; cart's state is its location and velocity. The simulation function 
;;; will do this repeatedly until the cart is centered or we hit a 
;;; pre-defined time limit.

(ns fungp.sample.cart
  (:use fungp.core)
  (:use fungp.util)
  (:use clojure.pprint))

;;; #### Some constants

(def MAX_T "Max time limit" 5)
(def STEP_INC "Step increment" 0.02)
(def TEST_POINTS "Number of test cases" 40)
(def PENALTY "Add this to fitness if program runs out of time" 200)
(def CRITERIA "Stop if fitness is lower than this value." 65)

;;; #### The simulation functions
;;;
;;; These functions are designed to test a given cart controlling function to
;;; see if it is able to center the cart. 

(defn cart-at-rest?
  "Is the cart at rest? This function tests its location, x, and velocity, v,
   to see if it is stopped in the center of the track."
  [x v]
  (and (<= (Math/abs x) 0.01) (<= (Math/abs v) 0.01)))

(defn out-of-time?
  "Has the simulation hit the maximum time limit? This function compares the
   amount of time, t, in the simulation so far to the maximum time, MAX_T."
  [t]
  (>= t MAX_T))

(defn get-thrust
  "Get the thrust of the cart by running the generated function with 
   velocity v and position x as inputs."
  [f v x]
  (if (> (f v x) 0) 1.0 -1.0))

(defn update-t
  "Update the t (time) value in the simulation by incrementing it by STEP_INC."
  [t]
  (+ t STEP_INC))

(defn update-v
  "Update the v (velocity) value in the simulation given the current thrust."
  [v thrust]
  (+ v (* thrust STEP_INC)))

(defn update-x
  "Update the x (position) value in the simulation given the current x 
   and v (velocity)"
  [x v]
  (+ x (* v STEP_INC)))

(defn move-cart
  "Simulate cart movement with the evolved function used to calculate
   thrust. Parameters are: t (time), v (velocity), x (position), 
   and f (function). This function loops until it is out of time or the cart is
   centered and at rest. On each iteration of the loop it updates the
   values of t, v, and x."
  [t v x f]
  (loop [t (float t) v (float v) x (float x)]
    ;; Explicitly calling "float" like this allows the Clojure compiler to unbox
    ;; the floating point values in the loop body.
    (cond (cart-at-rest? x v) t
          (out-of-time? t) PENALTY
          :else (recur (update-t t)
                       (update-v v (get-thrust f v x))
                       (update-x x v)))))

;;; #### Test data

(defn rand-spread
  "Range of starting position or velocity"
  [l] (repeatedly l #(- (* 1.5 (rand)) 0.75)))

(def cart-functions "Functions available to the evolved programs"
  '[[+ 2][* 2][- 2][fungp.util/abs 1][fungp.util/gt 2]])

(def cart-terminals "Terminals available to the evolved programs"
  '[x v -1])

(defn cart-error
  "Calculate error using move-cart, given a function and sequences of
   starting velocities and positions."
  [f velocity position]
  (reduce + (map (fn [v x] (move-cart 0 v x f))
                 velocity
                 position)))

(defn cart-test
  "Test an evolved function on random starting configurations."
  [f] (let [err (cart-error f (rand-spread TEST_POINTS) (rand-spread TEST_POINTS))]
        (if (> err CRITERIA) err 0)))

;;; #### Some existing solutions
;;;
;;; These are some examples to compare your outputs to.

(defn cart-optimal "The solution according to physics"
  [v x] (gt (* -1 x) (* v (abs v))))

(defn cart-suboptimal "A non-optimal evolved program"
  [v x] (gt (- x x) (+ (+ x (+ v x)) x)))

;;; #### Initializing fungp

(defn cart-fitness
  "Compute fitness for cart problem."
  [tree] (let [f (eval (list 'fn '[v x] tree))]
           (cart-test f)))

(defn cart-report
  "Reporting function. Prints out the tree and its score"
  [tree fitness]
  (pprint (list 'fn '[v x] tree))
  (print (str "Error:\t" fitness "\n\n"))
  (flush))

(defn test-cart
  "Run the cart problem"
  [n1 n2]
  (println "Cart Problem")
  (println "Error is number of seconds taken total across all the tests.")
  (println (str "If a program times out it is assigned " PENALTY " seconds."))
  (println (str "Because the simulation randomizes the test inputs, "
                "error values will go up and down during a search."))
  (let [options {:iterations n1 :migrations n2 :num-islands 4
                 :population-size 30 :terminals cart-terminals
                 :tournament-size 3 :mutation-probability 0.15
                 :functions cart-functions :fitness cart-fitness
                 :report cart-report
                 :max-depth 4 :mutation-depth 3}
        [tree score] (rest (run-genetic-programming options))]
    (do (println "Done!")
        (cart-report tree score))))

