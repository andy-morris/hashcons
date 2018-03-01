{-# LANGUAGE
    DeriveGeneric, DeriveAnyClass, DerivingStrategies,
    GeneralizedNewtypeDeriving
  #-}

import Data.HashCons
import GHC.Generics

newtype Id' = Id' String
  deriving newtype  (Eq, Hashable)
  deriving anyclass HashCons

type Id = HC Id'


-- subterms are also hashconsed, so that fast equality is immediately available
-- while actually processing the AST
data Expr' =
    Var Id
  | Lam Id Expr
  | App Expr Expr
  deriving stock    (Eq, Generic)
  deriving anyclass (Hashable, HashCons)

type Expr = HC Expr'


main :: IO ()
main = pure ()
