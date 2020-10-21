

-- |
-- = Solutions to Exercises for October 12, 2018
module S01 (
  -- ** Exercise 3
  x,

  -- ** Exercise 4
  nth,

  -- ** Exercise 5
  fromTo,

  -- ** Exercise 6
  allTrue
  ) where

{-| The previous version was
 
> x = mod length data Y
>    where
>      { data = [1..10] Y = 5 }

containing the following errors:

  1. @data@ is a keyword and is therefore not allowed as variable name.

  2. In the last line we are using braces (@{@ and @}@) to disregard the layout
     rule. But then items need to be separated by semicolons (@;@).

  3. @Y@ is not allowed as variable name (since it is uppercase).

  4. Missing parentheses around @length data@, since it is supposed to be the
     first argument of @mod@.
-}
x :: Int
x = mod (length d) y
  where
    { d = [1..10]; y = 5 }

{-| @nth@ corresponds to the Prelude function '!!', that is, we have

prop> nth xs i = xs !! i

whenever @i@ is between @0@ and the length of @xs@.
-}
nth :: [a] -> Int -> a
nth xs i = head (drop i xs)

{-| Just to get some intuition: for integer lists of the shape @[0..k]@,
@fromTo@ satisfies the property

prop> fromTo [0..k] i j = [i..j]

for all @0 <= i <= j <= k@.
-}
fromTo :: [a] -> Int -> Int -> [a]
fromTo xs i j = drop i (take (j+1) xs)

{-| @allTrue@ corresponds to the Prelude function 'and', that is, we have

prop> allTrue bs = and bs

for all lists of boolean values @bs@.
-}
allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (b:bs) = if b then allTrue bs else False

