% Amortized Analysis

# Queue example

```haskell
data Queue a = Queue [a] [a]

-- | Invariant on Queue
-- l can only be empty if r is
checkf (Queue [] r) = Queue (reverse r) []
checkf q            = q

empty                  = Queue [] []
isEmpty (Queue [] [])  = True
isEmpty _              = False
snoc (Queue l r) a     = Queue l (a:r)
head (Queue (a:_) _)   = a
tail (Queue (_:l) r)   = checkf $ Queue l r
```

#

`snoc`{.haskell} and `head`{.haskell} - $O(1)$

`tail`{.haskell} - $O(n)$

Can show that `tail`{.haskell} takes $O(1)$ amortized time

# Banker's method

* Every element in `l`{.haskell} is a single credit
* Every `snoc`{.haskell} to non-empty queue costs 1 and allocates a credit
* Every `tail`{.haskell} that does not reverse the rear takes 1 step
* Every `tail`{.haskell} that does reverse the rear takes m + 1 steps and spends m credits
	* Amoritzed cost of m + 1 - m = 1

# Physicist's method

* Define potential method $\Phi$ to be length of r (rear list)
* Every `snoc`{.haskell} takes 1 step and increases potential by 1
* Every `tail`{.haskell} that does not reverse the rear takes 1 step and leave potential unchanged
* Every `tail`{.haskell} that does reverse the rear takes m + 1 steps and decreases potential by m
	* Amortized cost of m + 1 - m = 1

# Apply it to delta graphs

* Possible leads
	* Potential function ($\Phi$): number of mutations in a tree
		* Insert takes time of 1 and increases potential by 1
	* Identify the costly operations and consider for amortization
		* topbacks - are there any instructions that build up to topbacks
		* an edge removal must be present
		* what triggers the topbacks?
