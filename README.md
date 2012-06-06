About this program
------------------

**fungp** is a genetic programming library implemented in the Clojure programming language. It's pronounced fun-gee-pee, for *functional genetic programming*. 

It's written in a literate programming style and is heavily documented. [Read the documentation here](http://gaia.ecs.csus.edu/~vollmerm/gp/).

How do I use it?
-----------------

See sample.clj for an example usage. The general idea is to pass in training data, the functions and symbols you want the generated programs to use, and the parameters you want the algorithm to use. More detail is provided in core.clj.

Here's an example of how to invoke the main function:

```clojure
(run-gp {:gens iter :cycles cycle
         :pop-size 6 :forest-size 50
         :symbols symbols :funcs funcs
         :term-max 1 :term-min -1
         :max-depth 4 :min-depth 2
         :repfunc repfunc  :reprate 1
         :mutation-rate 0.1 :tournament-size 5
         :actual actual :tests testdata})
```

The following options keywords are accepted:

 * *pop-size* --- the number of forests, and the number of top-level threads to run
 * *forest-size* --- the number of trees in each forest
 * *symbols* --- a sequence of symbols to be placed in the generated code as terminals
 * *funcs* --- a sequence (following a certain format; see core.clj or sample.clj) describing the functions to be used in the generated code
 * *term-max* and *term-min* --- the range of number terminals to be used in generated code (default to 1 and -1, respectively)
 * *depth-max* and *depth-min* --- the minimum/maximum height of randomly generated trees (defaults to 2 and 1, respectively)
 * *repfunc* --- the reporting function, which gets passed the best-seen individual (a hash with keys :tree and :fitness; see sample.clj for an example)
 * *reprate* --- the reporting rate; every nth cycle repfunc will be called
 * *mutation-rate* --- a number between 0 and 1 that determines the chance of mutation (defaults to 0.05)
 * *tournament-size* --- the number of individuals in each tournament selection (defaults to 5)
 * *tests* --- test inputs for your function, in the form of a sequence of vectors (each should match the length of *symbols* above)
 * *actual* --- the correct outputs for each of the *tests* elements

What does it do?
----------------

Here's a sample output from sample.clj:

```
fungp :: Functional Genetic Programming in Clojure
Mike Vollmer, 2012
==================================================

Attempting to match this function:
(fn [a] (+ 1 (+ (sin a) a)))

Lower numbers are better. Results shown are sum of error. Best so far:

Code:	(fn [a] (+ (cos -1) a))
Error:	84.6391402476599

Code:	(fn [a] (- a -1))
Error:	76.4219514139867

Code:	(fn [a] (+ (cos (cos (* a (sin a)))) (+ (sin a) a)))
Error:	26.815311352785372

Code:	(fn [a] (+ (sin (sin (+ (+ 0 a) 0))) (+ (+ 0 a) (cos (sin (* (cos (cos (- 0 a))) 0))))))
Error:	8.111235956013786

Done!
(fn [a] (+ (cos (sin (- a a))) (+ (sin a) a)))
Lowest error: 0.0
```

License
-------

Project created by Mike Vollmer and released under GPL. See the LICENCE file distributed with this code. 
