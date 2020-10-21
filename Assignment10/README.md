# Assignment 10

1. Read the lecture notes about type checking and type inference.
   
2. Use type checking to prove ``{f :: a -> b; y :: a} |- (\x.f x) y :: b``.
   
3. Solve the unication problem ``a2 ~ a1 -> a0; a3 ~ Int -> a2; a2 ~ a3`` and compute the resulting mgu, if possible. Check your result using unify.

4. Solve the unication problem ``Pair(Bool; a0) ~ Pair(a1; Int)``. Check
your solution using unify.

5. Use type inference to obtain the most general type of the expression ``map (\x: x)`` with respect to the environment
``E = { map :: (a -> b) -> List(a) -> List(b)}``.

6. Given the following types for environments and inference problems
``` haskell
type Env = [(String, Type)]
type IP = [(Env, Exp, Type)]
```
implement a function ``toUp :: IP -> UP`` that transforms a given type inference problem into the corresponding unication problem according to our typing constraint rules.