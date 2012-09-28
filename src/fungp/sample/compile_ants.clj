;;; ### Santa Fe Ant Trail
;;;
;;; This is an implementation of Koza's Santa Fe Ant Trail
;;; that uses fungp's local variables and result wrappers.
;;; Unlike the interpret-ants implementation, this one
;;; takes advantage of the JVM by compiling the individuals
;;; into JVM bytecode.

(ns fungp.sample.compile-ants
  (:use fungp.core)
  (:use fungp.util)
  (:use clojure.pprint))


;;; First some constants need to be established.

(def MAXSTEPS "Maximum number of steps before penalty"
  50)

(def PENALTY "Penalty applied to individuals that don't eat all the food"
  50)

(def MAXITER "Maximum number of iterations (length of loop in fitness function)"
  50)

(def STARTFOOD1 "Locations of food to be eaten."
  [[1 1][1 2][1 3][1 4][1 5][2 5][3 5][4 5][5 5][5 4]
   [5 3][6 3][7 3][8 3][8 4][7 4][6 4][6 2][7 2][8 3]
   [8 4][9 4][9 5][9 6][9 8][8 8][8 7][9 7]])

(def STARTFOOD2 "Second set of locations of food to be eaten."
  [[0 1][1 1][1 2][2 2][3 2][3 3][4 3][5 4][5 5][5 6]
   [6 6][7 6][8 6][9 6][9 7][9 8][9 9][8 9][7 9][6 9]
   [6 8][5 8][5 7][4 7][4 6][4 5][4 4][4 3]])

(def TESTFOOD "New set of food to test for generalization"
  [[1 0][2 0][3 0][4 0][5 0][6 1][6 2][6 3][6 4][6 5]
   [6 6][5 6][4 6][4 7][3 7][2 7][1 7][0 7][0 6][0 5]
   [1 5][2 5][2 4][2 3][2 1][3 1][3 2][3 3]])

;;; Vars in Clojure are thread-local, which is a useful property. Here
;;; a bunch of dynamic vars are declared, and they can be referenced by
;;; the rest of the code in this file. Once fungp has been started, there
;;; will be separate versions of these vars in each thread, and so the
;;; code that references them will refer to the local copy in the
;;; respective thread.
;;;
;;; Basically, you can write your application as if these were global
;;; variables, and they will work in a multithreaded environment
;;; automatically, like magic, as long as you include the "binding"
;;; statement below in the fitness function

(def ^:dynamic ant-x)
(def ^:dynamic ant-y)
(def ^:dynamic ant-dir)
(def ^:dynamic food)
(def ^:dynamic eaten)
(def ^:dynamic steps)

;;; Then some helper functions.

(defn sense-food [dir x y food]
  (cond (= dir 0) (some #(= % [x (inc y)]) food)
        (= dir 1) (some #(= % [(inc x) y]) food)
        (= dir 2) (some #(= % [x (dec y)]) food)
        (= dir 3) (some #(= % [(dec x) y]) food)
        :else (throw (Throwable. "Invalid direction"))))

(defmacro sense [x y]
  `(if (sense-food ant-dir ant-x ant-y food)
    ~x ~y))

(defn new-direction [elm ant-dir]
  (cond (= elm 'left)
        (cond (= ant-dir 0) 3
              (= ant-dir 1) 0
              (= ant-dir 2) 1
              (= ant-dir 3) 2)
        (= elm 'right)
        (cond (= ant-dir 0) 1
              (= ant-dir 1) 2
              (= ant-dir 2) 3
              (= ant-dir 3) 0)
        :else ant-dir))

(defn left []
  (set! ant-dir (new-direction 'left ant-dir)))

(defn right []
  (set! ant-dir (new-direction 'right ant-dir)))

(defn new-x [ant-dir ant-x]
  (cond (= ant-dir 1) (inc ant-x)
        (= ant-dir 3) (dec ant-x)
        :else ant-x))

(defn new-y [ant-dir ant-y]
  (cond (= ant-dir 0) (inc ant-y)
        (= ant-dir 2) (dec ant-y)
        :else ant-y))

(defn new-food [ant-x ant-y food]
  (remove #(= % [ant-x ant-y]) food))

(defn new-eaten [ant-x ant-y food eaten]
  (if (some #(= % [ant-x ant-y]) food) (inc eaten) eaten))

(defn move []
  (do
    (set! steps (inc steps))
    (set! ant-x (new-x ant-dir ant-x))
    (set! ant-y (new-y ant-dir ant-y))
    (set! eaten (new-eaten ant-x ant-y food eaten))
    (set! food (new-food ant-x ant-y food))))

(def ant-terminals '[(left) (right) (move)])
(def ant-functions '[[sense 2][do 2][do 3]])

(defn run-ant
  "This evaluates the fitness of the individuals. It uses a binding block to create
  thread-local bindings, evaluates the evolved code, and runs it in a loop."
  [tree init-food]
  (binding [ant-x 0 ant-y 0 ant-dir 0 food init-food eaten 0 steps 0]
    (let [func (eval (list 'fn [] tree))]
      (loop [iter MAXITER]
        (do
          (func) ;; run the code
          (if (zero? (count food))
            steps
            (if (or (zero? iter) (zero? steps) (> steps MAXSTEPS))
              (+ MAXSTEPS PENALTY (count food))
              (recur (dec iter)))))))))


(defn ant-fitness
  "Tests the fitness of an ant using run-ant and STARTFOOD."
  [tree] (+ (run-ant tree STARTFOOD1) (run-ant tree STARTFOOD2)))

(defn ant-report
  "Reporting function. Prints out the tree and its score"
  [tree fitness]
  (pprint (nth tree 2))
  (println (str "Error:\t" fitness "\n\n")))

(defn test-compile-ants [n1 n2]
  (println "Ant problem, from Koza")
  (println (str "Generations: " (* n1 n2)))
  (println "Training is done on two data sets, each with a penalty of 50 and 50 max steps.")
  (let [options {:iterations n1 :migrations n2 :num-islands 8
                 :tournament-size 3
                 :population-size 20 :max-depth 5
                 :terminals ant-terminals :fitness ant-fitness
                 :functions ant-functions :report ant-report}
        [tree score] (rest (run-genetic-programming options))]
    (do (println "Done!")
        (ant-report tree score)
        (println "Testing for generalization (one data set, same penalty and max steps)")
        (println (str "Score: " (run-ant tree TESTFOOD))))))

