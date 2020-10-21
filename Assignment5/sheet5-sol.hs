

-- |
-- = A Module for Lambda-Terms.
module Term (
  Id,
  Term(..),
  redexes,
  vars,
  fvs,
  freshName,
  subst,
  showsPrecTerm,
  ) where

import Data.List
import Data.Maybe (fromJust)

-- | Identifiers as @String@s.
type Id = String
-- | Lambda terms: variables, application, abstraction.
data Term = Var Id | App Term Term | Abs Id Term

-- | Compute all redexes of a given term.
redexes :: Term -> [Term]
redexes t@(App (Abs _ u) v) = t : redexes u ++ redexes v
redexes (App t u) = redexes t ++ redexes u
redexes (Var _) = []
redexes (Abs x t) = redexes t


{-|
The set of all variables occurring in a term. For example

> vars (App (Var "x") (Abs "y" (Var "y"))) == ["x", "y"]
 -}
vars :: Term -> [Id]
vars (Var x) = [x]
vars (App t u) = vars t `union` vars u
vars (Abs x t) = [x] `union` vars t

{-|
The set of free variables occurring in a term. For example

> fvs (App (Var "x") (Abs "y" (Var "y"))) == ["x"]
 -}
fvs :: Term -> [Id]
fvs (Var x) = [x]
fvs (App t u) = fvs t `union` fvs u
fvs (Abs x t) = fvs t \\ [x]

-- infinite list of variants of a given string
variants :: String -> [String]
variants [] = [[]]
variants [x] = [[y] | y <- [x .. 'z']] ++ map (x:) (variants "a")
variants (x:xs) = map (x:) $ variants xs

-- apply a renaming function to all variables of a term
rename :: (Id -> Id) -> Term -> Term
rename r (Var y) = Var (r y)
rename r (App t u) = App (rename r t) (rename r u)
rename r (Abs x t) = Abs (r x) (rename r t)

{-|
Given a variable @x@ and a set of variable names to avoid @ys@, @freshName x ys@
computes a pair @(x', ren)@ such that @x'@ is a variable that is fresh for @ys@
(that is, different from all elements of @ys@) and @ren :: Term -> Term@ is a
function that renames all occurrences of @x@ in a term by @x'@.
 -}
freshName :: Id -> [Id] -> (Id, (Term -> Term))
freshName x ys = (x', ren)
  where
    x' = head $ dropWhile (`elem` ys) $ variants x
    ren = rename (\v -> if v == x then x' else v)

{-|
Applying a substition @[x := s]@ to a lambda term @t@. For example (note how the
bound variable @"y"@ is renamed in order to avoid variable capture)

> subst "x" (Var "y") (Abs "y" (App (Var "x") (Var "y"))) == Abs "z" (App (Var "y") (Var "z"))
 -}
subst :: Id -> Term -> Term -> Term
subst x s (Var y) = if x == y then s else Var y
subst x s (App t u) = App (subst x s t) (subst x s u)
subst x s u@(Abs y t) = if x `elem` fvs u then Abs y' (subst x s (ren t)) else u
  where
    -- variables to avoid in fresh name
    taboo = [x] `union` (fvs s `union` (vars t \\ [y]))
    (y', ren) = freshName y taboo

{-|
Custom @Show@ instance for readability.-}
instance Show Term where
  showsPrec = showsPrecTerm

-- | @showsPrecTerm n t@ transforms term @t@ within a context of precedence @n@
-- into a "string" (for efficiency reasons actually the type 'ShowS' is used).
showsPrecTerm :: Int -> Term -> ShowS
showsPrecTerm d t | isChurchNum t = shows $ fromJust $ churchNum t
showsPrecTerm d (Var x) = showString x
showsPrecTerm d (App t u) = showParen (d > app_prec) $
  showsPrecTerm app_prec t .
  showString " " .
  showsPrecTerm (app_prec + 1) u
  where
    app_prec = 5
showsPrecTerm d (Abs x t) = showParen (d > abs_prec) $ showString "\\" . showsAbs x t
  where
    abs_prec = 4
    showsAbs x (Abs y t) = showString x . showString " " . showsAbs y t
    showsAbs x t = showString x . showString ". " . showsPrecTerm abs_prec t

churchNum :: Term -> Maybe Int
churchNum (Abs f (Abs x t)) = power f x t
  where
    power _ x (Var y) = if x == y then Just 0 else Nothing
    power f x (App (Var g) t) =
      if f == g then case power f x t of
        Nothing -> Nothing
        Just n -> Just $ n + 1
      else Nothing
    power _ _ _ = Nothing
churchNum _ = Nothing

isChurchNum t = churchNum t /= Nothing

