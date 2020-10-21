

-- |
-- = Solutions to Exercises for November 23, 2018
module S04 (
  -- ** Exercise 2
  {-| Using the definitions
   
   @
   iterate f x = x : iterate f (f x)

   tail (_:xs) = xs

   take n xs | n <= 0 || null xs = []
   take n (x:xs) = x : take (n-1) xs
   @

   The expression @take 4 (iterate tail [1..3])@ can be evaluated as follows:
   
   @
   take 4 (iterate tail [1..3])
     = take 4 ([1..3] : iterate tail (tail [1..3]))
     = [1..3] : take 3 (iterate tail (tail [1..3]))
     = [1..3] : take 3 (tail [1..3] : iterate tail (tail (tail [1..3])))
     = [1..3] : tail [1..3] : take 2 (iterate tail (tail (tail [1..3])))
     = [1..3] : [2,3] : take 2 (iterate tail (tail [2,3]))
     = [1..3] : [2,3] : take 2 (tail [2,3] : iterate tail (tail (tail [2,3])))
     = [1..3] : [2,3] : tail [2,3] : take 1 (iterate tail (tail (tail [2,3])))
     = [1..3] : [2,3] : [3] : take 1 (iterate tail (tail [3]))
     = [1..3] : [2,3] : [3] : take 1 (tail [3] : iterate tail (tail (tail [3])))
     = [1..3] : [2,3] : [3] : tail [3] : take 0 (iterate tail (tail (tail [3])))
     = [1..3] : [2,3] : [3] : tail [3] : []
     = [1..3] : [2,3] : [3] : [] : []
     = [[1,2,3],[2,3],[3],[]]
   @

   -}

  -- ** Exercise 3
  -- | 'Calc.Cmd', 'Calc.exec'

  -- ** Exercise 4
  levels,

  -- ** Exercise 5
  -- | "NL"

  -- ** Exercise 6
  -- | "Calc"
  ) where
import BTree

isEmpty :: BTree a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- | Compute the levels of a binary tree.
levels :: BTree a -> [[a]]
levels Empty = []
levels t =
  map (map root) $
  takeWhile (not . null) $
  iterate (concatMap subtrees) [t]
  where
    root (Node x _ _) = x
    subtrees Empty = []
    subtrees (Node _ l r) = filter (not . isEmpty) [l, r]

