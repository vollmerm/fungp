(comment
  [:title (fungp test)
   :author (Mike Vollmer)
   :license GPL])

(ns fungp.test.core
  (:use [fungp.core])
  (:use [clojure.test]))

; it would probably be a good idea to move test-gp and
; the test funcs/symbols to this file

(def testfuncs [{:op * :arity 2 :name '*}
                {:op + :arity 2 :name '+} 
                {:op - :arity 2 :name '-} 
                {:op inc :arity 1 :name 'inc} 
                {:op dec :arity 1 :name 'dec}])

(deftest test-conv-code
  (is (= '(* 1 2) (conv-code (list * 1 2) testfuncs))))

(deftest test-gp-returns-function
  (is (fn? (test-gp 1 1))))
