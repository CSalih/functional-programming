

-- |
-- = Sets as Sorted Lists
module SetAsSortedList (Set, empty, insert, mem, union, diff, toList) where
import qualified Data.List as List

-- | Sets are represented by sorted lists.
newtype Set a = Set { rep :: [a] }

-- | Obtain the underlying list.
toList :: Set a -> [a]
toList = rep

instance (Ord a, Show a) => Show (Set a) where
  show s = "{" ++ List.intercalate "," (elts s) ++ "}" 
    where elts = map show . toList

-- | The empty set.
empty :: Set a
empty = Set []

-- | Checking for set membership.
mem :: Ord a => a -> Set a -> Bool
mem x = elem x . rep

-- | Inserting single elements into 'Set's.
insert :: Ord a => a -> Set a -> Set a
insert x (Set ys) = Set $ insort x ys
  where
  insort x [] = [x]
  insort x (y:ys) = case compare x y of
    EQ -> y:ys
    LT -> x:y:ys
    GT -> y : insort x ys

-- | The union of two 'Set's.
union :: Ord a => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set $ merge xs ys
  where
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys) = case compare x y of
      EQ -> merge xs (y:ys)
      LT -> x : merge xs (y:ys)
      GT -> y : merge (x:xs) ys

-- | The difference between two 'Set's.
diff :: Ord a => Set a -> Set a -> Set a
diff (Set xs) (Set ys) = Set $ xs List.\\ ys

