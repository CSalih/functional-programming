# Assignment 1

1. Read
http://haskell.org/haskellwiki/Functional_programming and
http://haskell.org/haskellwiki/Haskell_in_5_steps.
2. Work through lessons 1 to 3 on http://tryhaskell.org/.
3. Find, explain, and correct the 4 errors in the following code:
x = mod length data Y
where
``{ data = [1..10] Y = 5 }``
4. Implement a function nth, where nth xs i yields the is element of
the list xs, in terms of the Prelude functions from this lecture.  
**Example:** ``nth ["a","b","c"] 1 = "b"``
5. Implement a function fromTo, where fromTo xs i j yields the part
of xs between positions i and j, in terms of the Prelude functions
from this lecture.  
**Example:** ``fromTo ["a","b","c","d"] 1 2 = ["b","c"]``
6. Use recursion to implement a function allTrue that, given a list of
boolean values, checks whether they are all true.  
**Example:** ``allTrue [True,False,True] = False``