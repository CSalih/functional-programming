# Assignment 8

1. Read the lecture notes on effciency,
https://wiki.haskell.org/Tail_recursion, and
http://en.wikipedia.org/wiki/Tail_recursion#
Tail_recursion_modulo_cons

2. Find one function that is tail recursive, one that is guardedly
recursive, and one that is neither, in the lecture slides of the earlier
weeks. In each case, justify your answer.

3. Use tupling to implement a more effcient version of
``initLast xs = (init xs, last xs)`` and prove by induction that
it coincides with initLast on non-empty lists.

4. Give a tail recursive implementation of the factorial function
``` haskell
fac n | n <= 1 = 1
      | otherwise = n * fac (n - 1)
```
and prove by induction that it coincides with ``fac``.

5. Use [LeanCheck](https://github.com/rudymatela/leancheck/blob/master/doc/tutorial.md) to test whether
``` haskell
sort' xs = foldr insort [] xs
where insort x [] = [x]
      insort x (y:ys)
          | x < y = x:y:ys
          | x == y = x:ys
          | otherwise = y : insort x ys
```
is a correct sorting function.  
**Hint:** Implement two functions ``sorted :: Ord a => [a] -> Bool``
and ``count :: Eq a => [a] -> a -> Int`` and use them to express
the desired property.

6. Give a tail recursive variant of
``nfold n f x = head $ drop n $ iterate f x``
and use LeanCheck to test whether it coincides with nfold.  
**Hint:** Import Test.LeanCheck.Function to test properties of
higher-order functions.