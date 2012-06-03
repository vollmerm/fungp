;;; Sample use
;;; ----------
;;;
;;; This demonstrates a sample use of **fungp**.

(ns fungp.sample
  (:use fungp.core)
  (:use clojure.pprint))

;;; Safe versions of inverse and divide to avoid divide-by-zero errors.

(defn inv [x]
  (if (zero? x) 0
      (/ 1 x)))

(defn sdiv [x y]
  (if (zero? y) 0
      (/ x y)))

;;; java.lang.Math methods cannot be higher-order functions, so they must
;;; be wrapped in clojure functions.

(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn tan [x] (Math/tan x))


;;; Functions are defined in a vector of maps like this.
(def funcs [{:op * :arity 2 :name '*}
            {:op + :arity 2 :name '+} 
            {:op - :arity 2 :name '-}
            {:op sdiv :arity 2 :name 'sdiv}
           ;{:op inc :arity 1 :name 'inc} 
           ;{:op dec :arity 1 :name 'dec}
           ;{:op inv :arity 1 :name 'inv}
           ;{:op sin :arity 1 :name 'sin}
           ;{:op cos :arity 1 :name 'cos}
            {:op tan :arity 1 :name 'tan}])

(def symbols ['a])

(def symbtest '(fn [a] (sdiv (* a a) (tan a))))

(def testfit (eval symbtest))

(def rtests (range -150 150))

(def testdata (map vector rtests))

(defn repfunc [{tree :tree fitness :fitness} par]
  (when par
    (do
      (print "Code:\t")(print (conv-code tree funcs))(print "\n")
      (print "Error:\t")(print fitness)(print "\n\n"))))

 (defn test-gp [cycle iter]
  (println "fungp :: Functional Genetic Programming in Clojure")
  (println "Mike Vollmer, 2012")
  (println "==================================================\n")
  (println "Attempting to minimize this function:")
  (pprint symbtest)
  (println "\nLower numbers are better. Results shown are sum of error. Best so far:\n")
  (def results (run-gp {:gens iter :cycles cycle
                        :pop-size 6 :forest-size 50
                        :symbols symbols :funcs funcs
                        :const [1 -1] :depth [4 2]
                        :rep [repfunc 1] :mutation 0.1 :tournament 5
                        :test [testfit testdata]}))
  (def best-result (:best results))
  (def out-func (list 'fn symbols (conv-code (:tree best-result) funcs)))
  (println "Done!")
  (println out-func)
  (print "Lowest error: ")(print (:fitness best-result))(print "\n")
  (eval out-func))
