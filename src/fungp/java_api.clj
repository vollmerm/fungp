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
  (:import (java.util HashMap))
  (:gen-class
    :name fungp.GPSearch
    :methods [^{:static true} [runSearch [java.util.HashMap] Object]
              ^{:static true} [compileTree [Object Object] Object]
              ^{:static true} [applyTree [Object Object] Object]]))

(defn replace-key-value
  "Apply a function to a value in a map and assoc the result."
  [hmap key value f]
  (assoc hmap key (f (hmap key))))

(defn replace-values-for-keys
  "Apply replace-key-value for multiple keys."
  [hmap keys f]
  (map #(replace-key-value hmap % f) keys))

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
      (replace-values-for-keys
       [:functions :terminals :numbers]
       #(map symbol (seq %)))
      (replace-values-for-keys
       [:fitness :report]
       #(symbol %))))

;;; Actually running the trees is complicated, as Java has no support for anonymous
;;; functions. As I said above, none of this code is tested so don't expect it to
;;; work yet.
(defn -runSearch 
  "The static method for running the search, which takes a HashMap as a parameter."
  [hmap]
  (run-genetic-programming (translate-hmap hmap)))

(defn -compileTree
  "Attempt to compile and run a tree of code."
  [tree parameters]
  (compile-tree tree parameters))

(defn -applyTree
  "Use apply to run a function that was compiled with compileTree."
  [func args]
  (apply func args))
