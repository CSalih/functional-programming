

module Typing (
    mgt, -- most general type with respect to a given environment
    infer,
    toUp
  ) where
import CoreFP
import Unification
import Data.Maybe

-- first environment for constants, second one for variables
type IP = (Env, Exp, Type)

maxIdx :: Type -> Int
maxIdx (TVar i)    =  i
maxIdx (TCon _ ts) =  maximum (0 : map maxIdx ts)

incBy :: Int -> Type -> Type
incBy i (TVar j)    =  TVar (i + j)
incBy i (TCon g ts) =  TCon g (map (incBy i) ts)

typeOf :: String -> Env -> Type
typeOf id env =
  fromMaybe (error ("no type for '" ++ id ++ "'")) $
    lookup id env

-- | Turn a type inference problem into a unification problem.
toUp :: IP -> UP
toUp (env, e, tau) = go (maxIdx tau + 1) [(env, e, tau)]
  where
    go i [] = []
    go i (ip:ips) = up ++ go i' (ips' ++ ips)
      where
        (i', up, ips') = step i ip
        step :: Int -> IP -> (Int, UP, [IP])
        step i (env, Var x, tau) = (i, [(typeOf x env, tau)], [])
        step i (env, Con c, tau) = (maxIdx tau' + 1, [(tau', tau)], [])
          where
            tau' = incBy i $ typeOf c env
        step i (env, App e1 e2, tau) = (i+1, [],
          [(env, e1, TVar i ~> tau),
           (env, e2, TVar i)])
        step i (env, Abs x e, tau) = (i+2,
          [(tau, TVar i ~> TVar (i+1))],
          [((x, TVar i) : env, e, TVar (i+1))])
        step i (env, Let x e1 e2, tau) = (i+1, [],
          [(env, e1, TVar i),
           ((x, TVar i) : env, e2, tau)])
        step i (env, Ite e1 e2 e3, tau) = (i, [],
          [(env, e1, tbool),
           (env, e2, tau),
           (env, e3, tau)])

mgt :: IP -> Type
-- use "seq" to force evaluation of "mgu", otherwise every
-- ground "t" is accepted as most general type without even
-- executing "unify", since then "mgu" is not needed in order
-- to compute the result of "tsub mgu t"
-- (The wonders of lazy evaluation.)
mgt ip@(_, _, t) = mgu `seq` tsub mgu t
  where
    mgu = unify $ toUp ip

infer :: String -> Type
infer s = mgt (primitives, e, TVar 0)
  where
    e = fromString s

