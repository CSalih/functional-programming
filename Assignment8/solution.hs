

-- |
-- = Solutions to Exercises for January 11, 2019
module S08 (
  -- * Exercise 2
  -- | 
  --     * The function drop from Lecture 3, given by
  --
  --         @
  --         drop :: Integer -> [a] -> [a]
  --         drop n xs | n <= 0  =  xs
  --         drop _ []           =  []
  --         drop n (_ :xs)      =  drop (n - 1) xs
  --         @
  --         is tail recursive, since the only recursive call is the last
  --         operation in the function body.
  --
  --     * The function 'L02.range' of Lecture 2 is guardedly recursive, since
  --     the only recursive call takes place in the second argument of the data
  --     constructor ':'.
  --     * 'L02.mySum' from the same lecture is neither tail recursive nor
  --     guardedly recursive, since the last operation is addition (using '+'),
  --     which is not a data constructor.

  -- * Exercise 3
  initLast',

  -- * Exercise 4
  fac',

  -- * Exercise 5
  sort',
  prop_sort,

  -- * Exercise 6
  nfold', prop_nfold,
  ) where
import Test.LeanCheck
import Test.LeanCheck.Function
import qualified L02


initLast :: [a] -> ([a], a)
initLast xs = (init xs, last xs)

-- | We prove the property @P(xs) = (xs /= [] ==> initLast xs = initLast' xs)@
-- by induction on @xs@.
-- 
-- * We start with the base case @xs = []@: since the premise of @P([])@ is @[]
-- /= []@ the claim holds vacuously.
-- * In the step case @xs = y:ys@ and @P(ys)@ by IH. In principle, we may assume
--
--     @
--     y:ys /= []
--     @
--     since the statement we want to show (@P(y:ys)@) is an implication.
--     However, this assumption trivially reduces to @True@ and hence
--     doesn't help us in our proof.
--
--     We continue by a case analysis on @ys@, which is either empty or not:
--
--     * If @ys = []@ then
--
--         @
--         initLast [y] = (init [y], last [y]) = ([], y) = initLast' [y]
--         @
--         and we are done.
--
--     * Otherwise, @ys /= []@ and hence by IH @initLast ys = initLast' ys@,
--     which implies @(init ys, last ys) = initLast'@.  We conclude by the
--     derivation:
--
--         @
--         initLast (y:ys)
--         = (init (y:ys), last (y:ys))
--         = (y : init ys, last ys)
--         = (y : us, u) where (us, u) = initLast' ys -- by IH
--         = initLast' (y:ys)
--         @
initLast' :: [a] -> ([a], a)
initLast' [x] = ([], x)
initLast' (x:xs) = (x:ys, y)
  where (ys, y) = initLast' xs

fac :: Integer -> Integer
fac n | n <= 1    = 1
      | otherwise = n * fac (n - 1)

-- | We prove the property @P(n) = (fac n * k = go k n)@ for arbitrary @k@, by
-- induction on @n@.
--
-- * We start with the base case @n = 0@. Then
--
--     @
--     fac 0 * k = k = go k 0
--     @
--
-- * In the step case we try to show @P(n+1)@ under the IH that @P(n)@ holds for
-- arbitrary values of @k@. We proceed by a case analysis on @n@:
--
--     * If @n = 0@ then @fac 1 * k = k = go k 1@.
--
--     * Otherwise @n > 0@ and hence:
--
--         @
--         fac (n+1) * k
--         = (n+1) * fac n * k
--         = fac n * ((n + 1) * k)
--         = go ((n + 1) * k) n -- by IH
--         = go k (n + 1)
--         @
--
-- In the end, from @P(n)@ for all @n@, we trivially obtain that @fac n = fac'
-- n@ for all @n@.
fac' :: Integer -> Integer
fac' = go 1
  where
    go acc n | n <= 1    = acc
             | otherwise = go (n * acc) (n - 1)


-- | A potential sorting function.
sort' :: Ord a => [a] -> [a]
sort' xs = foldr insort [] xs
  where insort x [] = [x]
        insort x (y:ys)
          | x <  y    = x:y:ys
          | x == y    = x:ys
          | otherwise = y : insort x ys

-- | The property
-- 
-- prop> prop_sort y xs
--
-- does not hold, e.g., for @y = 0@ and @xs = [0,0]@.
-- Hence, 'sort'' is not a correct sorting function.
prop_sort :: Int -> [Int] -> Bool
prop_sort y xs =
  sorted (sort' xs) && count (sort' xs) y == count xs y
  where
    sorted (x:y:ys) = x <= y && sorted (y:ys)
    sorted _ = True
    count [] y = 0
    count (x:xs) y | x == y    = count xs y + 1
                   | otherwise = count xs y

nfold n f x = head $ drop n $ iterate f x

-- | @nfold' n f x@ iteratively for @n@ steps applies the given function @f@ to
-- the argument @x@
nfold' n f x | n < 1     = x
             | otherwise = nfold' (n - 1) f (f x)

-- | The two implementations of n-fold function application behave the same:
--
-- prop> nfold n f x = nfold' n f x
--
prop_nfold :: Int -> (Int -> Int) -> Int -> Bool
prop_nfold n f x =
  n >= 0 ==> nfold n f x == nfold' n f x

