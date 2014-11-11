{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
module LCBound where

import Bound
import Control.Applicative
import Data.Traversable
import Data.Foldable

data Exp a =
    Var a
  | (Exp a) :@ (Exp a)
  | Lam (Scope () Exp a)
  deriving (Functor, Foldable, Traversable)  

instance Applicative Exp where
  pure = Var
  f <*> (Var a) = fmap ($ a) f
  f <*> (e1 :@ e2) = (f <*> e1) :@ (f <*> e2)
  f <*> Lam scope = Lam $ scope >>>= (\x -> fmap ($ x) f)

instance Monad Exp where
  return = Var
  (Var a) >>= f = f a
  e1 :@ e2 >>= f = (e1 >>= f) :@ (e2 >>= f)
  Lam scope >>= f = Lam (scope >>>= f)

