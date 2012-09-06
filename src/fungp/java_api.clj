;;; A Simple Java API
;;; -----------------
;;;
;;; This is a very basic attempt at a Java API for *fungp* based around using strings
;;; like symbols. It is not even close to idiomatic Java but should be enough to get
;;; *fungp* running in a Java program.
;;;
;;; That said, none of this code is tested at all and you shouldn't expect any of it
;;; to work.

(ns fungp.java-api
  "This declares not only the current namespace but the class to be generated as
   the external Java API."
  (:use fungp.core)
  (:use fungp.util)
  (:use clojure.walk)
  (:use clojure.pprint)
  (:use [clojure.string :only (split)])
  (:import (java.util HashMap))
  (:gen-class
    :name fungp.GPSearch
    :methods [^{:static true} [runSearch [java.util.HashMap] Object]
              ^{:static true} [compileTree [Object Object] 
                               clojure.lang.IFn]
              ^{:static true} [applyTree [clojure.lang.IFn Object] Object]]))

(defn replace-key-value
  "Apply a function to a value in a map and assoc the result."
  [hmap key f]
  (assoc hmap key (f (get hmap key))))

(defn replace-values-for-keys
  "Apply replace-key-value for multiple keys."
  [hmap keys f]
  (if (empty? keys) hmap
    (recur (replace-key-value hmap (first keys) f)
           (rest keys)
           f)))

;;; In order for the strategy of "strings to symbols of static methods" to work, we need to
;;; execute the proper import statements. So the API allows for the user to submit a sequence
;;; of strings representing classes to import. The key for this is "java-imports."
(defn eval-import-statements
  "Generate and eval import statements passed in via the Java API."
  [imports]
  (map eval (map #(list 'import (list 'quote (symbol %))) imports)))

(defn function-string-to-vector
  "Takes a string like \"+ 2\" and converts it to [+ 2]"
  [s]
  (let [s (seq (split s #"\s+"))]
    [(symbol (first s)) (Integer/parseInt (second s))]))


;;; The idea is to have strings representing keywords and symbols. First all the string
;;; keys are replaced with keywords, and then the instances where symbols were used
;;; in the values are replaced with symbols. To include (for example) a fitness function
;;; written in Java, you could provide a string containing the fully qualified path to
;;; a static Java method for the "fitness" value in the HashMap.
(defn translate-hmap
  "Translate the hash map passed in from (presumably) Java into something usable by fungp."
  [hmap]
  (-> hmap
      (keywordize-keys)
      (replace-key-value
       :terminals
       #(map symbol %))
      (replace-key-value
       :functions
       #(vec (map function-string-to-vector %)))
      (replace-key-value
       :numbers
       #(let [nums (seq %)] (if (nil? nums) [] nums)))
      (replace-key-value
       :report
       #(fn report [a b] (eval `(~(symbol %) (quote ~a) ~b))))
      (replace-key-value
       :fitness
       #(fn fitness [a] (eval `(~(symbol %) (quote ~a)))))))

;;; Actually running the trees is complicated, as Java has no support for anonymous
;;; functions. 
(defn -runSearch
  "The static method for running the search, which takes a HashMap as a parameter."
  [hmap]
  (let [hmap (into {} hmap)]
    (do (eval-import-statements (get hmap :java-imports))
      (let [hmap (translate-hmap hmap)]
        (run-genetic-programming hmap)))))

(defn -compileTree
  "Attempt to compile and run a tree of code."
  [tree parameters]
  (eval (list 'fn (vec (map symbol parameters)) tree)))

(defn -applyTree
  "Use apply to run a function that was compiled with compileTree."
  [func args]
  (apply func args))
