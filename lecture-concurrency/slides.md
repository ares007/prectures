% Concurrency in Haskell
% Philip Dexter
% \today

# Easy

Concurrency in Haskell should be easy

* No global state
* No need for locks

# Parallel Haskell

* deterministic
	* no race conditions
* no explicit synchronisation

# Simple example

	data Grid = ...
	solve :: Grid -> Maybe Grid
	grids = [...]

	complete = map solve grids

# Automatic?

Can it happen automatically?

# How far do we evaluate a value?

# Eval monad

* `rpar`{.haskell} - "my argument could be evaluated in parallel"
* `rseq`{.haskell} - "evaluate my argument now"

# Evaluate grids in two parts

	let (as, bs) = splitAt (length grids `div` 2) grids

	evaluate $ runEval $ do
	    a <- rpar (map solve as)
	    b <- rpar (map solve bs)
	    rseq a
	    rseq b
	    return ()

# Spark

	parMap :: (a -> b) -> [a] -> Eval [b]
	parMap f [] = return []
	parMap f (a:as) = do
	    b <- rpar (f a)
	    bs <- parMap f as
	    return (b:bs)

# Goal

Separate parallelism from the algorithm to be parallelised

# Strategies

	type Strategy a = a -> Eval a

	rseq :: Strategy a
	rpar :: Strategy a

# Using

	using :: a -> Strategy a -> a
	x `using` s = runEval (s x)

	x `using` s == x

	> 2 `using` rseq
	2
	> 2 `using` rpar
	2

# List strategy

	parList :: Strategy a -> Strategy [a]
	parList strat [] = return []
	parList strat (x:xs) = do
	    x' <- rpar (x `using` strat)
	    xs' <- parList strat xs
	    return (x':xs')

# Revisit parMap

	parMap f xs = map f xs `using` parList rseq

# Concurrent Haskell

* Asynchronous exceptions
* Software Transactional Memory (STM)

# Forking

	main = do
	    hSetBuffering stdout NoBuffering
	    forkIO (forever (putChar 'A'))
	    forkIO (forever (putChar 'B'))
	    threadDelay (10^6)

# MVar

An `MVar`{.haskell} is a lock

	data MVar a

	newEmptyMVar :: IO (MVar a)
	newMVar      :: a -> IO (MVar a)
	takeMVar     :: MVar a -> IO a
	putMVar      :: MVar a -> a -> IO ()

# Example

	do
	  m1 <- newEmptyMVar
	  m2 <- newEmptyMVar

	  forkIO $ do
	    r <- getURL "http://www.wikipedia.org/wiki/Shovel"
	    putMVar m1 r

	  forkIO $ do
	    r <- getURL "http://www.wikipedia.org/wiki/Spake"
	    putMVar m2 r

	  r1 <- takeMVar m1
	  r2 <- takeMVar m2
	  return (r1, r2)
