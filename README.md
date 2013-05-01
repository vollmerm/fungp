fungp
=====

This is the README for **fungp,** the genetic programming library I am working on as a student at [CSUS](http://csus.edu). The current version is 0.3.2.

If you're reading this as HTML, it was automatically generated from the README.md file in the root of the [source repository](https://github.com/probabilityZero/fungp).

What is genetic programming?
----------------------------

Genetic programming (GP) is the process of evolving computer programs using a process inspired by biological evolution. In GP, a computer program automatically writes new computer programs (in this case, by generating trees of new Clojure code), and judges them according to their ability to solve a problem or produce correct output. 

In **fungp,** like in many GP systems, tree structures representing programs go through processes inspired by biological evolution, such as mutation and reproduction. Their chance of reproduction is decided by their "fitness," which is assigned by a fitness function. For example, a simple problem you might solve with GP is symbolic regression, in which the evolved trees represent mathematical functions mapping one or more inputs to an output, and the fitness function would compile and run the evolved programs on known data to test whether sets of inputs produce correct output.

About this program
------------------

> There are only two hard things in Computer Science: cache invalidation,
> naming things, and off-by-one errors.
>
> --- *Paraphrased from Phil Karlton*

**fungp** is a genetic programming library implemented in the Clojure programming language. It's pronounced
fun-gee-pee, for *functional genetic programming* (or "genetic programming is fun").

A far more detailed explanation can be found in the core.clj source code. It is thoroughly documented. An
HTML document generated from the source is in the ```docs/``` folder, and [is available hosted on my school's
web server here](http://gaia.ecs.csus.edu/~vollmerm/gp/uberdoc.html). It's a good place to start and it includes a
getting started tutoral (the source of which is of course available under src/fungp).

How do I set it up?
-------------------

You'll need Clojure first, but that's easy. You can be up and running in seconds if you already have Java installed.

[First you install Leiningen.](https://github.com/technomancy/leiningen#installation) This is how you'd do that in a Unix-like system.

```
mkdir ~/bin
wget https://raw.github.com/technomancy/leiningen/preview/bin/lein
mv lein ~/bin
chmod 755 ~/bin/lein
lein repl
```

This assumes ```~/bin``` is on your ```$PATH```. The lein executable could go anywhere as long as it's on your path, but I'd recommend you put it somewhere in your user's home directory. Use ```echo $PATH``` to see what directories are on your path, or read [this article](http://www.cs.purdue.edu/homes/cs348/unix_path.html) to learn how to edit Unix environment variables.

Because you don't need root/sudo access to do this, it works fine on limited user accounts. To my friends at CSUS, that means it runs fine on Athena.

Once you have Clojure and Lein running you can grab fungp:

```
git clone https://github.com/probabilityZero/fungp.git
cd fungp
```

You can also list **fungp** as a dependency in your ```project.clj``` and make lein install it for you:

```
(defproject testfungp "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [fungp "0.3.2"]])
```

Then run ```lein deps``` and lein will download the library.

How do I use it?
----------------

See the samples for example usage (located in src/fungp/samples). The general idea is to pass in a fitness function, a terminal set, a function set, and other various options, and the algorithm will attempt to evolve code out of
the functions and terminals.

To use **fungp** you'll likely want to take advantage of Clojure's REPL (read-eval-print-loop). Once you have installed lein (see above), you can start the REPL with ```lein repl``` and start using **fungp.** For more detail on what to do from there, read the documentation (either in the ```docs/``` folder, in the source code, or on the website linked above). You can experiment with the included sampls or run the tutorial code like this:

```
=> (use 'fungp.tutorial)
nil
=> (test-genetic-program 5 10)
...
=> (use 'fungp.tutorial :reload-all) ;; reload file with changes
```

If you're starting out with **fungp** and Clojure, I recommend you start by copying and modifying the tutorial code or one of the samples. If you make a copy, be sure to give it a new name and namespace.

The **run-genetic-programming** function (the function you call to start the search) accepts the following options keywords:

 * iterations : number of iterations *between migrations*
 * migrations : number of migrations
 * num-islands : number of islands
 * population-size : size of the populations
 * tournament-size : size of the tournaments
 * mutation-probability : probability of mutation
 * mutation-depth : depth of mutation-generated sub-trees
 * max-depth : maximum depth of trees
 * terminals : terminals used in tree building
 * numbers : number literals to be used in tree building
 * functions : functions used in tree building, in the form [function arity]
 * adf-count : Number of automatically-defined functions
 * adf-arity : Number of arguments for automatically-defined functions
 * fitness : a fitness function that takes a tree and returns an error number, lower is better
 * report : a reporting function passed [best-tree best-fit] at each migration

Some minimal knowledge of Clojure is probably necessary to use this library well. Not to worry, though! Clojure is a rather nice language, with excellent documentation and many great tutorials (like [this one](http://java.ociweb.com/mark/clojure/article.html)).

What does it do?
----------------

Basically, fungp uses a process of evolution (mimicing natural selection in nature) to create and rewrite
Clojure code. Again, for a complete explanation look to fungp.core, or read [Wikipedia's explanation of 
Genetic Programming](http://en.wikipedia.org/wiki/Genetic_programming).

Here's a sample output from a symbolic regression problem. Reports are printed to the screen periodically, 
not every generation.

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

Links and References
--------------------

 * [Koza's GP Website](http://www.genetic-programming.org/)
 * [Dr. Gordon's Website at CSUS](http://gaia.ecs.csus.edu/~gordonvs/)
 * [A Field Guide to Genetic Programming](http://www.gp-field-guide.org.uk/)

License
-------

Project created by Mike Vollmer and released under GPL. See the LICENCE file distributed with this code.
