% Functional Programming
% Philip Dexter
% \today

# How to read these slides

* `'>'` denotes typing into the Haskell interpreter
```{.haskell .numberLines}
> print "hello"
"hello"
```
* all other code blocks signify files
```{.haskell .numberLines}
main = print "hello"
```
* comments in haskell are `'--'`{.haskell}

# What is functional programming?

* A programming paradigm
* Evaluate mathematical functions instead of executing statements
* Pure as possible - avoid state and mutability

# Purity

* Same input = same output -- guarantee
* No global variables
* Immutable data structures

# What is Haskell?

* Functional language
* Statically typed
* Non-strict

# Quick gotchas

* No loops
* No goto
* No real variables
* Wat

# Syntax

* Function application is written as juxtaposition
```{.haskell .numberLines}
> let f x = x + 1
> f 2
3
```
* Functions are `first-class citizens' -- higher order functions
```{.haskell .numberLines}
> let f x y = x y -- first argument is a function
> f print 2
2
```

# Lazy

* Really lazy
* Infinite list? no problem
```{.haskell .numberLines}
> let list = [1..]
> take 10 list
[1,2,3,4,5,6,7,8,9,10]
```
* Elements of `list` are only evaluated as needed

# Fibonacci

```{.haskell .numberLines}
> let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

**Remember**

* Lazy evaluation

**You need..**

* `zipWith (+)`{.haskell} merges two lists, element-wise, using addition
* `:`{.haskell} separates element of a list
* `tail`{.haskell} returns all but the first element of a list

# Visualized

```{.haskell}
fibs		= [0, 1, ..]
fibs		= [0, 1, ..]
tail fibs	= [1, ..]
```
. . .
```{.haskell}
fibs		= [0, 1, 1, ..]
fibs		= [0, 1, 1, ..]
tail fibs	= [1, 1, ..]
```
. . .
```{.haskell}
fibs		= [0, 1, 1, 2, ..]
fibs		= [0, 1, 1, 2, ..]
tail fibs	= [1, 1, 2, ..]
```
. . .
```{.haskell}
fibs		= [0, 1, 1, 2, 3, ..]
fibs		= [0, 1, 1, 2, 3, ..]
tail fibs	= [1, 1, 2, 3, ..]
```

# Bread n' butter

* Map - apply a function to every element of a list
```{.haskell .numberLines}
> map (*2) [1..10]
[2,4,6,8,10,12,14,16,18,20]
```
* Fold - fold a list into a single element
```{.haskell .numberLines}
> foldl (+) 0 [1..10]
55
```
* Filter - create a new list consisting of passing elements
```{.haskell .numberLines}
> filter even [1..10]
[2,4,6,8,10]
```
* Lambda - anonymous functions
```{.haskell .numberLines}
> map (\x -> x + 1) [1..10]
```

# Brain explode

**In reality**

* Every function only takes one argument
* What?
* What about `+`{.haskell} ?
```{.haskell .numberLines}
> let inc = (+) 1
> inc 2 -- same as (+) 1 2
3
> inc (inc (inc 2))
5
```
* Combine with higher order functions
```{.haskell .numberLines}
> let twice f x = f (f x)
> twice inc 0
2
> twice twice inc 1
5
```

# Composability

```{.haskell .numberLines}
> let do_things = map ((+) 1) . filter even . filter (<50) . filter (>20) . take 100
> do_things [1..]
```

# Brain explode

* Partially applying a function returns a new function
```{.haskell .numberLines}
> let listFromTo x y = [x..y]
> let from10to = listFromTo 10 -- returns a function
> from10to 20
[10,11,12,13,14,15,16,17,1,19,20]
```

# Definition

* A list is either
	* empty
	* an element plus a list
	* `:`{.haskell} is list cons operator (puts an element and a list together)
```{.haskell .numberLines}
> let empty = []
> let one = 1 : empty
> let two = 2 : one
> print two
[2,1]
```
	* inductive!

* Different definitions for different values
```{.haskell .numberLines}
length []		= 0
length (x:xs)	= 1 + length xs
```

# List comprehensions

```{.haskell .numberLines}
> let list = [1..10]
> let evens = [ x | x <- list, even x ]
> print evens
[2,4,6,8,10]
```

```{.haskell .numberLines}
> let list1 = [1,2,3]
> let list2 = [4,5,6]
> [ x + y | x <- list1, y <- list2 ]
[5,6,7,6,7,8,7,8,9]
```

# Algebra, stuff

* Algebraic data types
```{.haskell .numberLines}
data Expr =
	  I Int
	| Add Expr Expr
	| Mul Expr Expr
```
* Pattern match on type
```{.haskell .numberLines}
eval (I n)			= n
eval (Add e1 e2) 	= eval e1 + eval e2
eval (Mul e1 e2)	= eval e1 * eval e2
```

# Thanks

Questions?

# Resources

**Getting Haskell**

* http://www.haskell.org/platform/

**Buzzwords**

* Functors, monoids, monads
* Lenses

**Contact**

* philip.dexter@gmail.com
* github.com/philipdexter

# Monads

* ``Composable computation descriptions''

```{.haskell .numberLines}
data Maybe a = Nothing | Just a

ensureNotZero 0 = Nothing
ensureNotZero x = Just x
add1 x = Just (x + 1)

add1ToNotZero = ensureNotZero >=> add1
```

```{.haskell .numberLines}
> add1ToNotZero 0
Nothing
> add1ToNotZero 1
Just 2
```
