About this program
------------------

**fungp** is a genetic programming library implemented in the Clojure programming language. It's pronounced 
fun-gee-pee, for *functional genetic programming*. 

It's written in a literate programming style and is heavily documented. 
[Read the documentation here](http://gaia.ecs.csus.edu/~vollmerm/gp/).

How do I use it?
-----------------

See sample.clj for an example usage. The general idea is to pass in a fitness function, a terminal set,
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
 * functions : functions used in tree building, in the form [function arity]
 * fitness : a fitness function that takes a tree and returns an error number, lower is better
 * report : a reporting function passed [best-tree best-fit] at each migration

What does it do?
----------------

Here's a sample output from the test function in sample.clj:

```
fungp :: Functional Genetic Programming in Clojure
Mike Vollmer, 2012

==================================================

Matching function: (fn [x] (+ x (- (Math/sin x) (* x x))))
On inputs: [-5.0 -1.0 -0.5 0 0.5 1.0 5.0]

==================================================

Code:	(fn [a] (- (+ (+ a (Math/cos a)) (- a a)) (inc (* a a))))
Error:	5.193811787113044

Code:	(fn [a] (- (+ a (- (* a a) (* a a))) (- (* a a) (- a a))))
Error:	3.7149160597554607

Code:	(fn [a] (- (* (Math/sin a) (- (* (Math/sin (+ a a)) (inc (+ a a))) (Math/sin (+ a a)))) (* a a)))
Error:	3.6216421309983704

Done!
Code:	(fn [a] (- a (- (* a a) (Math/sin a))))
Error:	0.0
```

License
-------

Project created by Mike Vollmer and released under GPL. See the LICENCE file distributed with this code. 
