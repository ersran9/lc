{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
module LCBound where

import Bound
import Prelude.Extras
import Control.Applicative
import Data.Traversable
import Data.Foldable

data Exp a =
    V a
  | (Exp a) :@ (Exp a)
  | Lam (Scope String Exp a)
  deriving (Functor, Foldable, Traversable, Show, Eq, Read, Ord)  

instance Eq1 Exp
instance Ord1 Exp
instance Show1 Exp
instance Read1 Exp

instance Applicative Exp where
  pure = V
  f <*> (V a) = fmap ($ a) f
  f <*> (e1 :@ e2) = (f <*> e1) :@ (f <*> e2)
  f <*> Lam scope = Lam $ scope >>>= (\x -> fmap ($ x) f)

instance Monad Exp where
  return = V
  (V a) >>= f = f a
  e1 :@ e2 >>= f = (e1 >>= f) :@ (e2 >>= f)
  Lam scope >>= f = Lam (scope >>>= f)

pretty :: (Show a) => Exp a -> String
pretty (V a) = "Var: " ++ show a
pretty (e1 :@ e2) = "App: (" ++ pretty e1 ++ " :@ " ++ pretty e2 ++ ")"
pretty (Lam scope) = "Lam: " ++ show scope
