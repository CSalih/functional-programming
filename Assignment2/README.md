# Assignment 2

1. Read Chapters 1 and 2 of Real World Haskell.
2. Work through lessons 4 to 5 on http://tryhaskell.org/.
3. Give the types (and class constraints) for each of:
``` haskell
pair x y = (x, y)
tail2 xs = tail (tail xs)
triple x = x * 3
thrice f x = f (f (f x))
mapPair f (x, y) = (f x, f y)
idList = filter (const True)
```
4. Use equational reasoning to stepwise compute the result of
``filter (const False) ["a","b","c"]`` on paper.
5. Using ``foldr``, give alternative denitions of two of the functions we
have seen so far (excluding those that we already dened via foldr).
6. Dene a function ``intercalate :: [a] -> [[a]] -> [a]`` such
that ``intercalate xs xss`` inserts the list ``xs`` between the lists in
``xss`` and concatenates the result.  
**Example:** ``intercalate "; " ["one","two","six"] = "one; two; six"``