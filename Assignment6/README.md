# Assignment 6

1. Read Section 5 of the lecture notes on the lambda calculus.

2. Implement safe versions (that is, avoiding runtime errors using type
``Maybe``) of ``tail``, ``init``, and ``last``.
Examples:
``` haskell
safeTail [] = Nothing
safeLast [1..10] = Just 10
```
3. Reduce each of the following lambda-terms to NF, once using applicative
order reduction and once using normal order reduction:
<img src="https://render.githubusercontent.com/render/math?math=%5Cbegin%7Bequation%7D%0A(%5Clambda%20w.w)%20((%5Clambda%20xy.y)%20(z%20z))%20%5C%5C%0A(%5Clambda%20xy.%20x)%20(%5Clambda%20z.%20y%20z)%20%5C%5C%0A%5Clambda%20z.%20(%5Clambda%20x.%20x%20z%20y)%20(%5Clambda%20xy.%20y%20z)%20%5C%5C%0A%5Clambda%20xy.%20y%20(%5Clambda%20w.w)%20(%5Clambda%20yz.%20y%20x)%20%5C%5C%0A%5Cend%7Bequation%7D">

4. Implement single-step call by name reduction as a function ``cbn :: Term -> Maybe Term``(where the result is Nothing if no step is possible).
**Hint:** Start by implementing ``isWHNF :: Term -> Bool`` (checking whether a term is in WHNF) and ``root :: Term -> Maybe Term`` (performing a single root Beta-step, if possible).

5. Write an Eq class instance for the ``BTree`` based ``Set`` type of this
lecture (see also ``Set.hs``).

6. Give a ``Set`` implementation (supporting all functions of slide 14 based
on sorted lists). Make sure that each operation preserves the invariant
that the internal representation is in fact sorted.