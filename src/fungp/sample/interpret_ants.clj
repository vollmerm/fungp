;;; ### Interpreter-based Santa Fe Ant Trail
;;;
;;; This implementation of Koza's Santa Fe Ant Trail problem evolves
;;; an s-expression that is treated as a tree rather than as literal
;;; Clojure code. It is passed to a function that interprets it by
;;; traversing the evolved tree.
;;;
;;; This is one of the two examples of the Ant Trail program included with
;;; fungp. This example shows how you might approach a GP problem by
;;; constructing a machine and evolving a tree as a data structure to be
;;; understood by that machine, as opposed to evolving literal Clojure
;;; code. There are benefits and downsides to both approaches.

(ns fungp.sample.interpret-ants
  (:use fungp.core)
  (:use fungp.util)
  (:use clojure.pprint))

;;; First some constants need to be established.

(def MAXSTEPS 40)
(def STARTFOOD [[1 0][1 1][1 2][1 3][1 4][1 5][2 5][3 5][3 6][4 6]
                [5 6][6 6][7 7][7 8][8 8][9 8][9 9][10 9][10 10][9 10]
                [8 10][8 9][11 9][11 10]])
(def NUMFOOD (count STARTFOOD))

;;; Then some helper functions.

(defn sense-food [dir x y food]
  (cond (= dir 0) (some #(= % [x (inc y)]) food)
        (= dir 1) (some #(= % [(inc x) y]) food)
        (= dir 2) (some #(= % [x (dec y)]) food)
        (= dir 3) (some #(= % [(dec x) y]) food)
        :else (throw (Throwable. "Invalid direction"))))

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

(defn new-x [elm ant-dir ant-x]
  (if (= elm 'move)
    (cond (= ant-dir 1) (inc ant-x)
          (= ant-dir 3) (dec ant-x)
          :else ant-x)
    ant-x))

(defn new-y [elm ant-dir ant-y]
  (if (= elm 'move)
    (cond (= ant-dir 0) (inc ant-y)
          (= ant-dir 2) (dec ant-y)
          :else ant-y)
    ant-y))

(defn new-food [ant-x ant-y food]
  (remove #(= % [ant-x ant-y]) food))

(defn new-eaten [ant-x ant-y food eaten]
  (if (some #(= % [ant-x ant-y]) food) (inc eaten) eaten))

(defn simulate-ants
  "Simulate the ant movements by repeatedly interpreting the movement function."
  [iter full-tree tree ant-dir ant-x ant-y food eaten steps]
  (cond (zero? iter) (+ MAXSTEPS (count food))
        (and (seq? tree) (empty? tree))
        (recur (dec iter) full-tree full-tree ant-dir ant-x ant-y food eaten (inc steps))
        (empty? food) steps ;; ant ate all the food
        :else
        (cond (not (seq? tree))
              (recur iter full-tree '()
                     (new-direction tree ant-dir)
                     (new-x tree ant-dir ant-x)
                     (new-y tree ant-dir ant-y)
                     (new-food ant-x ant-y food)
                     (new-eaten ant-x ant-y food eaten)
                     steps)
              (= (first tree) 'sense)
              (let [tree (if (sense-food ant-dir ant-x ant-y food)
                           (nth tree 1) (nth tree 2))]
                (recur iter full-tree tree ant-dir ant-x ant-y food eaten steps))
              (= (first tree) 'do)
              (if (= (count (rest tree)) 1)
                (recur iter full-tree (nth tree 1) ant-dir ant-x ant-y food eaten steps)
                (recur iter full-tree (cons 'do (rest (rest tree)))
                       (new-direction (nth tree 1) ant-dir)
                       (new-x (nth tree 1) ant-dir ant-x)
                       (new-y (nth tree 1) ant-dir ant-y)
                       (new-food ant-x ant-y food)
                       (new-eaten ant-x ant-y food eaten)
                       steps))
              :else (throw (Throwable. (str "Unexpected element in tree: " tree))))))



(def ant-terminals '[move left right])

(def ant-functions '[[sense 2][do 2][do 3]])

(defn ant-fitness [tree]
  (simulate-ants MAXSTEPS (nth tree 2) (nth tree 2) 0 0 0 STARTFOOD 0 0))


(defn ant-report
  "Reporting function. Prints out the tree and its score"
  [tree fitness]
  (pprint (nth tree 2))
  (print (str "Error:\t" fitness "\n\n"))
  (flush))

(defn test-ants [n1 n2]
  (println "Ant problem\n")
  (println (str "Food count: " NUMFOOD "\n"))
  (let [options {:iterations n1 :migrations n2 :num-islands 8 :population-size 50
                 :max-depth 5 :mutation-depth 1
                 :terminals ant-terminals :fitness ant-fitness
                 :functions ant-functions :report ant-report}
        [tree score] (rest (run-genetic-programming options))]
    (do (println "Done!")
        (ant-report tree score))))

