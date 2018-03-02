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

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.HashCons
  (HashCons, hc, HC, getVal)
where

import Data.HashCons.HC
import Data.HashCons.Cache
import Data.Hashable

import System.IO.Unsafe

import Foreign


-- | Types which support hash-consing.
--
-- There are some restrictions on types for which this class makes sense:
--
-- 1. The type must have no type variables: an instance for @T Int@ would be
--    fine, but not for @T a@. (There must also be no constraints, but that is
--    unlikely to be a problem if all instances are ground.)
-- 2. Equality and hashing must consider all data in a value. It need not
--    necessarily be structural equality, but a subterm should not simply be
--    ignored. (An example of why someone might want to do this is annotations
--    in an abstract syntax tree.)
class (Eq a, Hashable a) => HashCons a where
  hcCache :: Cache a
  hcCache = unsafePerformIO newCache
  {-# NOINLINE hcCache #-}

-- | Make a hash-consed value.
hc :: HashCons a => a -> HC a
hc x = unsafePerformIO $ lookupOrAdd x hcCache


-- | Reads an underlying value and caches it
instance (Read a, HashCons a) => Read (HC a) where
  readsPrec d = map (\(x, s) -> (hc x, s)) . readsPrec d

-- | Stores the underlying value, and re-caches it on retrieval
instance (Storable a, HashCons a) => Storable (HC a) where
  sizeOf    = sizeOf . getVal
  alignment = alignment . getVal
  peek      = fmap hc . peek . castPtr
  poke p    = poke (castPtr p) . getVal
