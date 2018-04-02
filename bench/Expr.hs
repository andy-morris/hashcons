{-# LANGUAGE
    ConstraintKinds, DataKinds, DeriveAnyClass, DeriveGeneric,
    DerivingStrategies, GADTs, GeneralizedNewtypeDeriving,
    UndecidableInstances
  #-}

module Expr where

import Data.HashCons
import Data.HashCons.Memo

import Data.Text (Text, pack)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import GHC.Generics (Generic)
import Control.DeepSeq


newtype Id = Id (HC Text)
  deriving newtype (Eq, Ord, Show, Hashable, MemoArg, NFData)

mkId :: String -> Id
mkId = Id . hc . pack
{-# INLINE mkId #-}


data ExprLayer a =
    Var !Id
  | Lam !Id !a
  | App !a  !a
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, HashCons, NFData)

class IsExpr a where
  make :: ExprLayer a -> a
  dest :: a -> ExprLayer a

{-# SPECIALISE make :: ExprLayer Expr -> Expr #-}
{-# SPECIALISE make :: ExprLayer ExprU -> ExprU #-}
{-# SPECIALISE dest :: Expr -> ExprLayer Expr #-}
{-# SPECIALISE dest :: ExprU -> ExprLayer ExprU #-}

type IsMemoExpr a = (IsExpr a, MemoArg a, CanFinalize a ~ 'True)

var :: IsExpr a => Id -> a
var = make . Var
{-# INLINE var #-}

lam :: IsExpr a => Id -> a -> a
lam x s = make $ Lam x s
{-# INLINE lam #-}

app :: IsExpr a => a -> a -> a
app s t = make $ App s t
{-# INLINE app #-}

apps :: IsExpr a => a -> [a] -> a
apps = foldl app
{-# INLINE apps #-}

newtype ExprU = U {unU :: ExprLayer ExprU}
  deriving newtype (Eq, Ord, Show, Hashable, NFData)

instance IsExpr ExprU where
  make = U
  {-# INLINE make #-}
  dest = unU
  {-# INLINE dest #-}


newtype Expr = E {unE :: HC (ExprLayer Expr)}
  deriving newtype (Eq, Ord, Show, Hashable, NFData, MemoArg)

instance IsExpr Expr where
  make = E . HC
  {-# INLINE make #-}
  dest (E (HC e)) = e
  {-# INLINE dest #-}


type Unrolled a = a -> a

type Subst a = a -> Id -> a -> a

subst' :: IsExpr a => Unrolled (Subst a)
subst' f s x e = case dest s of
  Var y   | x == y    -> e
          | otherwise -> s
  Lam y a | x == y    -> s
          | otherwise -> lam y (f a x e)
  App a b             -> app (f a x e) (f b x e)
{-# NOINLINE subst' #-}

substU :: IsExpr a => Subst a
substU = subst' substU

substM :: IsMemoExpr a => Subst a
substM = memo3 $ subst' substM


treeTerm :: IsExpr a => Int -> a
treeTerm 0 = var $ mkId "x"
treeTerm n = apps (var fn) (replicate 2 $ treeTerm (n - 1)) where
  fn = mkId $ "f" ++ show n

substXIn :: IsExpr a => Subst a -> a -> a
substXIn subst e = subst e (mkId "x") (var $ mkId "y")


type Fv a = a -> HashSet Id

fv' :: IsExpr a => Unrolled (Fv a)
fv' f s = case dest s of
  Var x -> HashSet.singleton x
  Lam x a -> HashSet.delete x (f a)
  App a b -> HashSet.union (f a) (f b)
{-# NOINLINE fv' #-}

fvU :: IsExpr a => Fv a
fvU = fv' fvU

fvM :: IsMemoExpr a => Fv a
fvM = memo $ fv' fvM
