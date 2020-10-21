# Assignment 5

1. Read the lecture notes on the lambda calculus until Section 5.
2. Encode the Haskell function ``boolToInt :: Bool -> Int``
``` haskell
boolToInt True = 1
boolToInt False = 0
```
as lambda-term and step-wise compute the NF of the lambda-term encoding
``` haskell
boolToInt True.
```
3. Give a combinator ``E ("eraser")`` that satises ``E t -> *ß E`` for arbitrary lambda-terms t.  
**Hint:** Fix ``E = A A`` and try to come up with an appropriate A.
4. Given the following Haskell type for lambda-terms
``` haskell
type Id = String
data Term = Var Id | App Term Term | Abs Id Term
```
implement a function ``redexes :: Term -> [Term]`` that computes
the list of all redexes occurring in a term.