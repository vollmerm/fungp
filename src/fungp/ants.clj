(ns fungp.ants
  (:use fungp.core)
  (:use fungp.util)
  (:require clojure.pprint))

(def MAXSTEPS 200)
(def STARTFOOD [[0 1][1 0][1 1][1 2][1 3][1 4][1 5][2 5][3 5][3 6][4 6][5 6][6 6][7 6][8 6]
                [9 6][10 6][11 6][11 5][11 4][12 4][13 4][13 5][13 6][13 7][13 8][13 9]
                [13 10][13 11]])
(def NUMFOOD (count STARTFOOD))

(defn ant-fitness [tree]
  (simulate-ants MAXSTEPS tree tree 0 0 0 STARTFOOD 0))

;;; 0: north, 1: east, 2: south, 3: west

(defn sense-food [dir x y food]
  (cond (= dir 0) (some #(= % [x (inc y)]) food)
        (= dir 1) (some #(= % [(inc x) y]) food)
        (= dir 2) (some #(= % [x (dec y)]) food)
        (= dir 3) (some #(= % [(dec x) y]) food)))

(defn simulate-ants [iter full-tree tree ant-dir ant-x ant-y food eaten]
  (cond (zero? iter) (+ MAXSTEPS (* 2 (+ (count food))))
        (empty? tree) (recur (dec iter) full-tree full-tree ant-dir ant-x ant-y food eaten)
        (empty? food) iter
        :else (let [tree (cond (= (first tree) 'sense)
                               (if (sense-food ant-dir ant-x ant-y food)
                                 (nth tree 1)
                                 (nth tree 2))
                               (= (first tree) 'do)
                               (rest tree)
                               :else tree)
                    cur (if (seq? tree) (first tree) tree)
                    nex (if (seq? tree) (rest tree) '())]
        (recur iter full-tree nex
                     (cond (= cur 'left) (if (= ant-dir 0) 3
                                                      (dec ant-dir))
                           (= cur 'right) (if (= ant-dir 3) 0
                                                       (inc ant-dir))
                           :else ant-dir)
                     (if (= cur 'move)
                       (cond (= ant-dir 1) (inc ant-x)
                             (= ant-dir 3) (dec ant-x)
                             :else ant-x)
                       ant-x)
                     (if (= cur 'move)
                       (cond (= ant-dir 0) (inc ant-y)
                             (= ant-dir 2) (dec ant-y)
                             :else ant-y)
                       ant-y)
                     (remove #(= % [ant-x ant-y]) food)
                     (if (some #(= % [ant-x ant-y]) food) (inc eaten) eaten)))))

(def ant-terminals '[move left right])

(def ant-functions '[[sense 2][do 2][do 3]])

(defn ant-report
  "Reporting function. Prints out the tree and its score"
  [tree fitness]
  (clojure.pprint/pprint (list 'fn '[a] tree))(print "\n")
  (print "Error:\t")(print fitness)(print "\n\n")
  (flush))

(defn test-ants [n1 n2]
  (println "Ant problem\n")
  (let [options {:iterations n1 :migrations n2 :num-islands 2 :population-size 200 :tournament-size 5 :mutation-probability 0.1
                 :max-depth 25 :terminals ant-terminals :fitness ant-fitness
                 :functions ant-functions :report ant-report
                 :adf-count 0}
        [tree score] (rest (run-genetic-programming options))]
    (do (println "Done!")
        (ant-report tree score))))
