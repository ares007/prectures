% Type classes
% Philip Dexter
% \today

# Task

Write a function that checks if an item is inside an array

# Elem

	elem :: a -> [a] -> Bool
	x `elem` []     = False
	x `elem` (y:ys) = x == y || (x `elem` ys)

# Let's try

	data Circle = Circle (Int, Int) Float
	circles = [Circle (1,1) 5, Circle (3,2) 2.2]

	> Circle (2,2) 1 `elem` circles
	...

# What went wrong?

We never told Haskell how to compare two circles

# Type classes

	class Eq a where
	    (==) :: a -> a -> Bool

	instance Eq Circle where
	    Circle (p1x, p1y) r1 == Circle (p2x, p2y) r2
	               | p1x == p2x && p1y == p2y && r1 == r2 = True
	               | otherwise = False

# Context

	> :t (==)
	(==) :: (Eq a) => a -> a -> Bool
	--        ^ context

Any type, `a`{.haskell}, given that it is an instance of the class `Eq`{.haskell}

# Elem revisited

	elem :: (Eq a) => a -> [a] -> Bool
	x `elem` []     = False
	x `elem` (y:ys) = x == y || (x `elem` ys)

Since `(==)`{.haskell} requires the constraint of `Eq`{.haskell}, we must as well

# Numeric type classes

	> :t 3
	3 :: Num a => a

3 is polymorphic

	> :t (+)
	(+) :: Num a => a -> a -> a

# Fractional

	> :t 3.3
	3.3 :: Fractional a => a
	> :t (3.3 +)
	(3.3 +) :: Fractional a => a -> a
	> 3.3 + (1 :: Int)
	...

# Class extension

`Ord`{.haskell} _inherits_ all of the operations in `Eq`{.haskell}

	class (Eq a) => Ord a where
	    (<), (<=), (>=), (>) :: a -> a -> Bool
	    max, min             :: a -> a -> a

Any type which is an instance of `Ord`{.haskell} must also be an instance of `Eq`{.haskell}

	quicksort :: (Ord a) => [a] -> [a]

# Higher order types

	data Tree a = Node (Tree a) a (Tree a)
	            | Empty

`Tree`{.haskell} is a type constructor

`Tree Integer`{.haskell} is a type

# Functor

	class Functor f where
	    fmap :: (a -> b) -> f a -> f b

# Plugging in

	> :t fmap
	fmap :: Functor f => (a -> b) -> f a -> f b
	fmap :: (a -> b) -> Tree a -> Tree b

# Tree as a functor

	instance Functor Tree where
	    fmap f (Node l n r) = Node (fmap f l) (f n) (fmap f r)
	    fmap _ (Empty)      = Empty

# Lists are functors

	instance Functor [] where
	    fmap f [] = []
	    fmap f (x:xs) = f x : xs

# Extra slides

# Elem v2

	elem x  []     = False
	elem x (y:ys) = x == y || (elem x ys)

# Possible exam question

What is the most general type for the function:

	whichOne (a, b) | a == b    = a
	                | otherwise = a + 1

# Answer

	whichOne :: (Eq a, Num a) => (a, a) -> a
