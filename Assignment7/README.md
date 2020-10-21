# Assignment 7

1. Read the lecture notes on reasoning about functional programs.

2. Prove ``map f (map g xs) = map (f ° g) xs`` for
``` haskell
map f [] = []
map f (x:xs) = f x : map f xs
```
3. Prove ``filter p (map f xs) = map f (filter (p ° f) xs)`` for
``` haskell
filter p [] = []
filter p (x:xs) | p x = x : filter p xs
| otherwise = filter p xs
```

4. Prove ``map f (xs ++ ys) = map f xs ++ map f ys`` for
``` haskell
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
```

5. With for all `xs` prove ``xs: take n (map f xs) = map f (take n xs)`` for
``` haskell
take n (x:xs) | n > 0 = x : take (n - 1) xs
take _ _ = []
```

6. With for all `xs` prove ``xs: take n xs ++ drop n xs = xs`` for
``` haskell
drop n (_:xs) | n > 0 = drop (n - 1) xs
drop _ xs = xs
```

## Alternatively
Choose two of the previous exercises and prove them with Isabelle/HOL
using the custom type
``` haskell
datatype 'a lst = NIL | CONS 'a "'a lst"
```
and your own implementations of the relevant functions among
``` haskell
map :: "('a => 'b) => 'a lst => 'b lst"
filter :: "('a => bool) => 'a lst => 'a lst"
app :: "'a lst => 'a lst => 'a lst"
take :: "nat => 'a lst => 'a lst"
drop :: "nat => 'a lst => 'a lst"
```