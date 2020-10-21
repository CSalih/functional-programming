

-- |
-- = Solutions to Exercises for November 9, 2018
module S02 (
  -- ** Exercise 3
  pair, tail2, triple, thrice, mapPair, idList,

  -- ** Exercise 4
  {-| Using the definition

   @
   filter p [] = []
   filter p (x:xs)
     | p x = x : filter p xs
     | otherwise = filter p xs
   @

   The expression @filter (const False) ["a","b","c"]@ can be evaluated as follows (where the
   justification for applying conditional equations resulting from guarded
   patterns are given on the right, separated by @<==@):

   @
   filter (const False) ["a","b","c"]
     = filter (const False) ["b","c"]   <== const False "a" /= True
     = filter (const False) ["c"]       <== const False "b" /= True
     = filter (const False) []          <== const False "c" /= True
     = []
   @
   -}

  -- ** Exercise 5
  reverse, map, concat,

  -- ** Exercise 6
  intercalate

  ) where
import Prelude hiding (reverse, map, concat)
import qualified Prelude (reverse, map, concat)

{-|

> pair x y = (x,y)

Since there are no restrictions on @x@ and @y@, the input of
'pair' are two arbitrary values of types @a@ and @b@. Constructing
a pair out of those values, results in the type @(a, b)@ and
thus @a -> b -> (a, b)@ for 'pair'.
 -}
pair :: a -> b -> (a,b)
pair x y = (x,y)

{-|

> tail2 xs = tail (tail xs)

Since 'tail' is of type @[a] -> [a]@, the input @xs@ as well as the result of
'tail2' both have to be lists with same element-type. Thus, 'tail2' is of
type @[a] -> [a]@.
 -}
tail2 :: [a] -> [a]
tail2 xs = tail (tail xs)

{-|

> triple x = x * 3

The operation '*' is of type @Num a => a -> a -> a@. Hence @x@, needs to
be of some 'Num'-type, resulting in @Num a => a -> a@ for 'triple'.
 -}
triple :: Num a => a -> a
triple x = x * 3

{-|

> thrice f x = f (f (f x))

Since @f@ is applied to the result of @f x@, the input as well as the
output of @f@ need to be of the same type as @x@. There are no further
restrictions. Hence the type of 'thrice' is @(a -> a) -> a -> a@.
 -}
thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))

{-|

> mapPair f (x, y) = (f x, f y)

The function @f@ is applied to both components @x@ and @y@ of the input pair.
Thus, both have to be of the same type. Moreover, the output pair consists of
two components that result from applying @f@. Again, this yields that both have
to be of the same type. There are no further restrictions. In particular, there
are not restrictions on @f@, except for it being a function. Hence, the type of
@f@ is @a -> b@. Taken together, this gives the type @(a -> b) -> (a, a) -> (b,
b)@ for 'mapPair'.
 -}
mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

{-|

> idList = filter (const True)

The constructor 'True' is of type 'Bool'. Moreover, 'const' is a function of
type @a -> b -> a@. Consequently, @const True@ is of type @b -> Bool@.
Furthermore, 'filter' has type @(a -> Bool) -> [a] -> [a]@. Which finally yields
the type @[b] -> [b]@ for 'idList'.
 -}
idList :: [b] -> [b]
idList = filter (const True)

{-| For 'reverse' we use the auxiliary function @snoc@ (\'cons\' spelled
backwards), which adds an element at the end of a list. Then, using 'foldr', we obtain

@
reverse = foldr snoc []
@
 -}
reverse :: [a] -> [a]
reverse = foldr snoc []
  where snoc x xs = xs ++ [x]

{-| For 'map' we use /function composition/ @(.)@ (where @(f . g) x = f (g x)@,
reading \"first apply @g@ and then apply @f@ to the result\") together with
the /cons/ function '(:)'. Using 'foldr', we obtain

@
map f = foldr ((:) . f) []
@
 -}
map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

{-| One possible definition is

@
concat []     = []
concat (x:xs) = x ++ concat xs
@

Again, we could use 'foldr' to obtain the shorter definition

@
concat = foldr (++) []
@
 -}

concat :: [[a]] -> [a]
{-
concat []       = []
concat (xs:xss) = xs ++ concat xss
-}
concat = foldr (++) []

{-|
Note the special treatment of /singleton lists/ (that is, lists having exactly
one element), which avoids the \"separator\" to be appended after the last list.
 -}
intercalate xs [] = []
intercalate xs [ys] = ys
intercalate xs (ys:yss) = ys ++ xs ++ intercalate xs yss

