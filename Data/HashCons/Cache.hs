-- |
-- Module:      Data.HashCons.Cache
-- Description: Hash-cons cache
-- Copyright:   © 2018 Andy Morris
-- Licence:     LGPL-3
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: weak pointers & finalizers

module Data.HashCons.Cache (Cache, newCache, lookupOrAdd) where

import Data.HashCons.HC
import Data.HashCons.MkWeak
import Data.HashCons.SeqSubterms

import Data.Hashable
import Data.HashTable.IO

import Control.Concurrent.MVar


type HashTable k v = BasicHashTable k v


newtype Cache a = C (MVar (HashTable a (CacheEntry a)))

type CacheEntry a = Weak (HC a)


newCache :: IO (Cache a)
newCache =
  fmap C $ newMVar =<< new

remove :: (Eq a, Hashable a) => a -> Cache a -> IO ()
remove x (C var) =
  withMVar var $ \cache ->
    delete cache x

newHC :: (Eq a, Hashable a) => a -> Cache a -> IO (Maybe (CacheEntry a), HC a)
newHC x c = do
  y   <- makeHC x
  ptr <- mkWeakPtr y (Just $ remove x c)
  pure (Just ptr, y)

lookupOrAdd :: (Eq a, Hashable a, SeqSubterms a) => a -> Cache a -> IO (HC a)
lookupOrAdd x c@(C var) =
  withMVar (seqSubterms x var) $ \cache ->
    mutateIO cache x $ \ent -> case ent of
      Nothing  -> newHC x c
      Just ptr -> deRefWeak ptr >>= \y' -> case y' of
        Nothing -> newHC x c
        Just y  -> pure (Just ptr, y)
