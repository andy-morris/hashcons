-- |
-- Module:      Data.HashCons.MkWeak
-- Description: The @MkWeak@ class
-- Copyright:   © 2018 Andy Morris
-- Licence:     AGPL-3
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: GHC internals
--
-- A class for types which can have weak pointers/finalizers usefully attached
-- to them.

{-# LANGUAGE
    BangPatterns, DataKinds, KindSignatures, MagicHash, UnboxedTuples,
    ScopedTypeVariables
  #-}

module Data.HashCons.MkWeak (Weak, Finalizer, MkWeak (..), deRefWeak) where

import GHC.Base  (IO (..), mkWeak#, mkWeakNoFinalizer#)
import GHC.MVar  (MVar (..))
import GHC.IORef (IORef (..))
import GHC.STRef (STRef (..))
import GHC.Weak  (Weak (..), deRefWeak)
import GHC.Exts  (TYPE, RuntimeRep (..))


type Finalizer = IO ()

class MkWeak k where
  mkWeak :: k -> v -> Maybe Finalizer -> IO (Weak v)

  mkWeakPtr :: k -> Maybe Finalizer -> IO (Weak k)
  mkWeakPtr x = mkWeak x x

  addFinalizer :: k -> Finalizer -> IO ()
  addFinalizer x fin = () <$ mkWeak x () (Just fin)


mkWeakUnlifted :: forall (k :: TYPE 'UnliftedRep) v.
                  k -> v -> Maybe Finalizer -> IO (Weak v)
mkWeakUnlifted k# v fin' = IO $ \s1 ->
  case fin' of
    Nothing ->
      let !(# s2, w #) = mkWeakNoFinalizer# k# v s1 in (# s2, Weak w #)
    Just (IO fin#) ->
      let !(# s2, w #) = mkWeak# k# v fin# s1 in (# s2, Weak w #)
{-# INLINE mkWeakUnlifted #-}

instance MkWeak (IORef a) where
  mkWeak (IORef (STRef r#)) = mkWeakUnlifted r#

instance MkWeak (MVar a) where
  mkWeak (MVar m#) = mkWeakUnlifted m#
