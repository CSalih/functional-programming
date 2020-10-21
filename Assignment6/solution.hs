

-- |
-- = Solutions to Exercises for December 7, 2018
module S06 (
  -- * Exercises 2
  safeTail, safeInit, safeLast,

  -- * Exericse 3
  -- | See <../pdfs/06.pdf>.

  -- * Exericse 4
  root, isWHNF, cbn,
  
  -- * Exercise 5
  equals,

  -- * Exercise 6
  -- | "SetAsSortedList"
  ) where
import BTree (BTree(..))
import qualified BTree
import Term
import Data.List
import Set

-- | Safe versions of 'tail' and 'init'.
safeTail, safeInit :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeInit [] = Nothing
safeInit (x:xs) = case safeInit xs of
  Nothing -> Just []
  Just ys -> Just $ x:ys

-- | Safe version of 'last'.
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:xs) = case safeLast xs of
  Nothing -> Just x
  y -> y

-- | Perform a single beta-step at the root.
root :: Term -> Maybe Term
root (App (Abs x t) u) = Just $ subst x u t
root _ = Nothing

-- | Check whether a given term is in /weak head normal form/.
isWHNF :: Term -> Bool
isWHNF (Var _) = True
isWHNF (Abs _ _) = True
isWHNF (App (Abs _ _) _) = False
isWHNF (App t _) = isWHNF t

-- | Single-step call by name reduction.
cbn :: Term -> Maybe Term
cbn t =
  if isWHNF t then Nothing
  else case root t of
    Nothing -> leftmost t
    u -> u
  where
    leftmost (App t u) = case cbn t of
      Just t' -> Just $ App t' u
      Nothing -> case cbn u of
        Just u' -> Just $ App t u'
        Nothing -> Nothing

-- | Test whether two given 'Set's are qual.
equals :: Ord a => Set a -> Set a -> Bool
equals s t = toList s == toList t

instance Ord a => Eq (Set a) where
  (==) = equals

