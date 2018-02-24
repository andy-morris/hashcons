-- |
-- Module:      Data.HashCons
-- Description: Hash-consing support
-- Copyright:   © 2018 Andy Morris
-- Licence:     LGPL-3
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: GHC internals; weak pointers & finalizers; stable names
--
-- Hash-consing, or interning, is a way to gain constant-time equality testing
-- (and hashing) for potentially-large data types such as strings or abstract
-- syntax trees. Internally a table of live values is kept, and
-- newly-constructed values are looked up in the table to check if they already
-- exist. If they do, then the existing one is reused (along with a tag). The
-- table is pruned using finalisers when these tagged values are garbage
-- collected.
--
-- This library should be thread- and exception-safe.

module Data.HashCons
  (HashCons, hc,
   HC, getVal,
   SeqSubterms (..),
   Hashable (..))
where

import Data.HashCons.HC
import Data.HashCons.Cache
import Data.HashCons.SeqSubterms

import Data.Hashable

import System.IO.Unsafe


-- | Types which support hash-consing.
class (Eq a, Hashable a, SeqSubterms a) => HashCons a where
  hcCache :: Cache a
  hcCache = unsafePerformIO newCache
  {-# NOINLINE hcCache #-}

-- | Make a hash-consed value.
hc :: HashCons a => a -> HC a
hc x = unsafePerformIO $ lookupOrAdd x hcCache
