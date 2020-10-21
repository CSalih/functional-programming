

-- |
-- = Solutions to Exercises for November 16, 2018
module S03 (
  -- ** Exercise 2
  -- | "Queue"

  -- ** Exercise 3
  ptriples,

  -- ** Exercise 4
  prefixes,
  suffixes,

  -- ** Exercise 5
  sublists,

  -- ** Exercise 6
  -- | "Year"
  smartStack,
  smartTile,
  ) where
import Picture
import Data.List

{-| The function 'ptriples' computes all so called /Pythagorean triples/ with
    components up to a certain size.
 -}
ptriples n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y+1..n], x^2 + y^2 == z^2]

{-| The function 'prefixes' computes all prefixes of a given list (including the
    list itself) in increasing order of length. -}
prefixes :: [a] -> [[a]]
prefixes [] = [[]]
prefixes (x:xs) = [] : map (x:) (prefixes xs)

{-| The function 'suffixes' computes all suffixes of a given list (including the
    list itself) in decreasing order of length. -}
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes (x:xs) = (x:xs) : suffixes xs

sublistsBySize :: [a] -> [[[a]]]
sublistsBySize [] = [[[]]]
sublistsBySize (x:xs) = zipWith (++) ([] : map (map (x:)) xss) (xss ++ [[]])
  where
    xss = sublistsBySize xs

{-| The function 'sublists' computes all sublists of a given list in increasing
    order of length. Here, a sublist is obtained from a list by omitting
    arbitrarily many of its arguments.
 -}
sublists :: [a] -> [[a]]
sublists = concat . sublistsBySize

{-| @center n p@ horizontally centers the picture @p@ inside a box of width @n@.
 -}
center :: Int -> Picture -> Picture
center n (h, w, css)
  | w <= n = (h, n, map centerline css)
  | otherwise = error ("picture of width " ++ show w ++
    " does not fit in box of width " ++ show n)
  where
    centerline l = left ++ l ++ right
    left = replicate (nc + nh - wh - wc) ' '
    right = replicate (nh - wh) ' '
    nc = if n `mod` 2 == 0 then 0 else 1
    wc = if w `mod` 2 == 0 then 0 else 1
    wh = w `div` 2
    nh = n `div` 2

{-| @centerv n p@ vertically centers the picture @p@ inside a box of height @n@.
 -}
centerv :: Int -> Picture -> Picture
centerv n (h, w, css)
  | h <= n = (n, w, top ++ css ++ bot)
  | otherwise = error ("picture of height " ++ show h ++
    " does not fit in box of height " ++ show n)
  where
    top = replicate (nc + nh - hh - hc) (replicate w ' ')
    bot = replicate (nh - hh) (replicate w ' ')
    nc = if n `mod` 2 == 0 then 0 else 1
    hc = if h `mod` 2 == 0 then 0 else 1
    hh = h `div` 2
    nh = n `div` 2

{-| A variant of 'above' that adapts the width of the smaller picture to the
width of the bigger one.
 -}
smartAbove :: Picture -> Picture -> Picture
smartAbove p q = center w p `above` center w q
  where
    w = max (width p) (width q)

{-| A variant of 'beside' that adapts the height of the smaller picture to the
height of the bigger one.
 -}
smartBeside :: Picture -> Picture -> Picture
smartBeside p q = centerv h p `beside` centerv h q
  where
    h = max (height p) (height q)

{-| Variants of 'stack' and 'spread' that work for pictures of different
    dimensions (by centering smaller pictures with respect to bigger ones).
 -}
smartStack, smartSpread :: [Picture] -> Picture
smartStack = foldl1 smartAbove
smartSpread = foldl1 smartBeside

{-| A variant of 'tile' that works for pictures of different dimensions
    (by centering smaller pictures with respect to bigger ones).
 -}
smartTile :: [[Picture]] -> Picture
smartTile = smartStack . map smartSpread

