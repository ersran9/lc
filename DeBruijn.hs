module DeBruijn where

import Data.Foldable
import Data.Monoid ((<>))
-- debruijn as nested notation paper
data Term a = Var a
            | Lam (Incr (Term a))
            | App (Term a) (Term a)
            deriving Show
                     
data Incr v = Succ (Incr v)
            | Zero
            deriving Show

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

-}
