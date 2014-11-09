module LC where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative 

data Term a = Var a
            | Lam a (Term a)
            | App (Term a) (Term a)
            deriving (Show, Eq)  

freeVars :: (Ord a) => Term a -> S.Set a
freeVars (Var a) = S.singleton a
freeVars (Lam a t) = S.delete a $ freeVars t  
freeVars (App t1 t2) = (freeVars t1) `S.union` (freeVars t2)

allVars :: (Ord a) => Term a -> S.Set a
allVars (Var a) = S.singleton a
allVars (Lam _ t) = allVars t
allVars (App t1 t2) = (freeVars t1) `S.union` (freeVars t2)

newtype IdInt  = IdInt { unId :: Int }

firstBoundId :: IdInt
firstBoundId = IdInt 0

firstUnboundId :: IdInt
firstUnboundId = IdInt (-1)

instance Show IdInt where
  show (IdInt i) | i < 0 = "f" ++ show (i * (-1))
                 | otherwise = "x" ++ show i

toIdInt :: (Ord a) => Term a -> Term IdInt
toIdInt t = evalState (conv t) ((firstBoundId, firstUnboundId), M.empty, freeVars t)

conv :: (Ord a) => Term a -> State ((IdInt, IdInt), M.Map a IdInt, S.Set a) (Term IdInt)
conv (Var a) = Var <$> (convVar a)
conv (Lam a b) = Lam <$> (convVar a) <*> (conv b)
conv (App a b) = App <$> (conv a) <*> (conv b)

convVar :: (Ord a) => a -> State ((IdInt, IdInt), M.Map a IdInt, S.Set a) IdInt 
convVar a = do
  (idI, m, s) <- get
  case a `M.lookup` m of
   (Just v) -> return v
   Nothing -> do
     let (idI', fId) = freshId idI (a `S.member` s)
     put (idI', M.insert a fId m, s)
     return fId
     
  where nextId (IdInt x) = IdInt (x+1)
        prevId (IdInt x) = IdInt (x-1)
        freshId (id1, id2) isFree = case isFree of
          True -> let t1 = prevId id1
                  in ((t1, id2), t1)
          False -> let t2 = nextId id2
                  in ((id1, t2), t2)
