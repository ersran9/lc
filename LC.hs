module LC where

import qualified Data.Set as S

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
