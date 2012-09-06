About this program
------------------

**fungp** is a genetic programming library implemented in the Clojure programming language. It's pronounced 
fun-gee-pee, for *functional genetic programming*.

A far more detailed explanation can be found in the core.clj source code. It is thoroughly documented. An
HTML document generated from the source is in the docs/ folder, and is available hosted on my school's
web server [here](http://gaia.ecs.csus.edu/~vollmerm/gp). It's a good place to start and it includes a 
getting started tutoral (the source of which is of course available under src/fungp).

How do I use it?
-----------------

See the samples for example usage (located in src/fungp/samples). The general idea is to pass in a fitness function, a terminal set,
a function set, and other various options, and the algorithm will attempt to evolve code out of 
the functions and terminals.

The **run-genetic-programming** function accepts the following options keywords:

 * iterations : number of iterations *between migrations*
 * migrations : number of migrations
 * num-islands : number of islands
 * population-size : size of the populations
 * tournament-size : size of the tournaments
 * mutation-probability : probability of mutation
 * mutation-depth : depth of mutated trees
 * max-depth : maximum depth of trees
 * terminals : terminals used in tree building
 * numbers : number literals to be used in tree building
 * functions : functions used in tree building, in the form [function arity]
 * adf-count : Number of automatically-defined functions
 * adf-arity : Number of arguments for automatically-defined functions
 * fitness : a fitness function that takes a tree and returns an error number, lower is better
 * report : a reporting function passed [best-tree best-fit] at each migration

What does it do?
----------------

Here's a sample output from the test function in sample.clj:

```
fungp :: Functional Genetic Programming in Clojure
Mike Vollmer, 2012
Test inputs: (-10 -8 -6 -4 -2 0 2 4 6 8)
Test outputs: (300.0 192.0 108.0 48.0 12.0 0.0 12.0 48.0 108.0 192.0)

(fn
 [a]
 (let
  []
  (-
   (+
    (- (inc (inc 9.0)) (fungp.util/sdiv (dec a) (* a 0.0)))
    (+ (- (fungp.util/abs a) (- a a)) (inc (* a a))))
   (dec (* (- (dec a) (inc a)) (+ (+ a 7.0) (* a a)))))))

Error:	10210.0

(fn
 [a]
 (let
  []
  (+
   (- (inc (inc 9.0)) (fungp.util/sdiv (dec a) (* a 0.0)))
   (+
    (+
     (inc (fungp.util/abs a))
     (+
      (+
       (- (inc (inc 9.0)) (fungp.util/sdiv (dec a) (* a 0.0)))
       (+ (- (fungp.util/abs a) (- a a)) (inc (* a a))))
      (fungp.util/abs
       (fungp.util/sdiv (fungp.util/abs 8.0) (dec 3.0)))))
    (inc (* a a))))))

Error:	6778.0

(fn
 [a]
 (let
  []
  (+
   (* a a)
   (+ (+ (- (fungp.util/abs a) (- a a)) (inc (* a a))) (inc (* a a))))))

Error:	580.0

(fn [a] (let [] (+ (* a a) (+ (* a a) (inc (* a a))))))

Error:	10.0

Done!
(fn [a] (let [] (+ (* a a) (+ (* a a) (* a a)))))

Error:	0.0
```

License
-------

Project created by Mike Vollmer and released under GPL. See the LICENCE file distributed with this code. 
