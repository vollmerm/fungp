(ns fungp.test.core
  (:use [fungp.core])
  (:use [clojure.test]))

(defn list-or-cons? [tree] 
  (or (list? tree)
      (= clojure.lang.Cons (type tree))))

(def test-functions '[[+ 2][- 2][* 2][inc 1][dec 1]])

(def test-terminals '[x])

(deftest test-create-tree
         (let [tree-type (type (create-tree 5 test-terminals [] test-functions :grow))]
           (is (or (= tree-type clojure.lang.Symbol)
                   (= tree-type clojure.lang.Cons)))))

(deftest test-module-tree
         (let [tree (create-module-tree 5 test-terminals [] test-functions 1 1 0 0 :grow)]
           (do (is (list-or-cons? tree))
               (is (= (first tree) 'let))
               (is (vector? (second tree))))))

(deftest test-tree-operations
         (let [tree (create-tree 5 test-terminals [] test-functions :fill)
               subtree (rand-subtree tree)]
           (do (is (seq? (replace-subtree tree '(+ x x)))))
               (is (seq? subtree))))

(deftest test-truncate
         (let [tree (create-tree 5 test-terminals [] test-functions :fill)]
           (do (is (= tree (truncate tree 10)))
               (is (not (= tree (truncate tree 2)))))))