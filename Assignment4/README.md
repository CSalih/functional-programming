# Assignment 4

1. Read Chapter 7 of Real World Haskell.
2. Evaluate the function call ``take 4 (iterate tail [1..3])`` by
equational reasoning using the denitions:
``` haskell
iterate f x = x : iterate f (f x)
tail (_:xs) = xs
take n xs | n <= 0 || null xs = []
take n (x:xs) = x : take (n-1) xs
```
3. Dene a new type Cmd of commands that work with respect to an
implicit stack and allow us to push an Int on top, pop the top
element, and add the topmost elements by popping both and pushing
the result on top. Moreover, implement a function
``exec :: Cmd -> Stack Int -> Stack Int`` that executes a single
command on a given stack.
4. Implement a function ``levels :: BTree a -> [[a]]`` that returns
the list of nodes at each level of a binary tree.  
**Example:**
``` haskell
levels (Node 1 (Node 2 Empty Empty)
        (Node 3 Empty Empty)) = [[1],[2,3]]
```