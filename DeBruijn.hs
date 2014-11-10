{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module DeBruijn where

import Data.Foldable
import Data.Monoid ((<>))
-- debruijn as nested notation paper

data BinTree a = Leaf a | Fork (BinTree a) (BinTree a)
               deriving Show
-- should preserve
-- fmap f . id == id . fmap f == fmap f
-- fmap (f . g) == fmap f . fmap g
instance Functor BinTree where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Fork f1 f2) = Fork (f `fmap` f1) (f `fmap` f2)

{-
for base case
---
(f. g) `fmap` (Leaf a) = Leaf (f . g $ a)
(f `fmap` g `fmap` (Leaf a)
= f `fmap` (Leaf (g a))
= Leaf (f . g $ a)

let f.g = h
fmap (f. g) (Fork f1 f2)
= Fork (h `fmap` f1) (h `fmap` f2)

(fmap f . fmap g) (Fork f1 f2)
= (fmap f) (Fork (g `fmap` f1) (g `fmap` f2))
= Fork (f `fmap` (g `fmap` f1)) (f `fmap` (g `fmap` f2))

-}

-- naturality condition
-- function f :: M a -> N a under conditions
-- fmap(N) k . f == f . fmap(M) k

-- obeys naturality laws
flatten :: BinTree a -> [a]
flatten (Leaf a) = [a]
flatten (Fork t1 t2) = (flatten t1) ++ (flatten t2)

instance Foldable BinTree where
  foldMap f (Leaf a) = f a
  foldMap f (Fork t1 t2) = foldMap f t1 <> foldMap f t2

-- foldMap (const (Sum 1))
-- foldMap (: [])

-- or for a more permissive idea
-- since its difficult to express non monoidal things in earlier version

foldB :: (a -> b) -> (b -> b -> b) -> BinTree a -> b
foldB f _ (Leaf a) = f a
foldB f c (Fork t1 t2) = (foldB f c t1) `c` (foldB f c t2)

{-
   Laws :
  foldB Leaf Fork == id  -- (1)
  foldB Leaf Fork (Leaf a)
= Leaf a

  foldB Leaf Fork (Fork a b)
= foldB Leaf Fork a `Fork` foldB Leaf Fork b
= Fork (foldB Leaf Fork a) (foldB Leaf Fork b)

  fusion law
  (h . l  = l', h . f = \a b -> f' (h a) (h b)) => h . foldB l f = foldB l' f' 

  map fusion law
  foldB l f . fmap h = foldB (l . h) f 
-}

instance Monad BinTree where
  return = Leaf
  (Leaf a) >>= f = f a
  (Fork t1 t2) >>= f = Fork (t1 >>= f) (t2 >>= f)

{-
   unit = return
   join :: m (m a) -> m a
   Laws:

   join . fmap unit = id
   join . unit = id
   join . fmap join = join . join

   joinB = foldB id Fork
-}

data Pair a = Pair a a
            deriving Show 

data Term a = Var a
            | Lam (Term (Incr a))
            | App (Pair (Term a))
            deriving Show
                     
data Incr v = Succ v
            | Zero
            deriving Show

instance Functor Incr where
  fmap f (Succ v) = Succ (f v)
  fmap _ Zero = Zero

instance Functor Term where
  fmap f (Var a) = Var $ f a
  fmap f (Lam a) = Lam (fmap (fmap f) a)
  fmap f (App a) = App (fmap (fmap f) a)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

foldT :: (forall a .a -> f a) ->
         (forall a .Pair (f a) -> f a) ->
         (forall a .f (Incr a) -> f a) ->
         Term b -> f b
         
foldT v _ _ (Var t) = v t
foldT v a l (Lam t) = l . (foldT v a l) $ t
foldT v a l (App p) = a . fmap (foldT v a l) $ p             

-- naturality law
-- fmap(N) k . foldT v a l = foldT v a l . fmap(T) k

gfoldT :: (forall a .m a -> f a) ->
         (forall a .Pair (f a) -> f a) ->
         (forall a .f (Incr a) -> f a) ->
         (forall a .Incr (m a) -> m (Incr a)) ->   
         Term (m b) -> f b
gfoldT v _ _ _ (Var a) = v a
gfoldT v a l k (App p) = (a . fmap (gfoldT v a l k)) p
gfoldT v a l k (Lam p) = (l . gfoldT v a l k . fmap k) p

-- monad definition

joinT :: Term (Term a) -> Term a
joinT = gfoldT id App Lam distT


distT :: (Incr (Term a)) -> (Term (Incr a))
distT Zero = Var Zero
distT (Succ v) = fmap Succ v

-- real deal
abstract :: Eq a => a -> Term a -> Term a
abstract x = Lam . fmap (match x)

match :: Eq a => a -> a -> Incr a
match x y = case x == y of
  True -> Zero
  False -> Succ y

apply :: Term a -> Term (Incr a) -> Term a
apply t = joinT . fmap (subst t . fmap Var)

subst :: a -> Incr a -> a
subst x Zero = x
subst _ (Succ y) = y
