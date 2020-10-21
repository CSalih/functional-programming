# Assignment 3

1. Read Chapter 3 of Real World Haskell.
2. Implement a module Queue for FIFO queues with interface:
``` haskell
empty :: Queue a
isEmpty :: Queue a -> Bool
enqueue :: a -> Queue a -> Queue a
dequeue :: Queue a -> (a, Queue a)
```
1. Use list comprehension to implement a function that generates the
list of all triples ``(x, y, z) :: (Int, Int, Int)`` such that
``x^2 + y^2 = z^2`` and ``1 <= x <= y < z <= n`` for given ``n``.
4. Implement two recursive functions
prefixes, ``suffixes :: [a] -> [[a]]`` that compute all prexes
and suffixes of a given list, respectively.  
**Examples:**
``` haskell
prefixes [1,2,3] = [[],[1],[1,2],[1,2,3]]
suffixes [1,2,3] = [[1,2,3],[2,3],[3],[]]
```