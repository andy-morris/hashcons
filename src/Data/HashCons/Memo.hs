-- |
-- Module:      Data.HashCons.Memo
-- Description: Memoisation via hash-consing
-- Copyright:   © 2018 Andy Morris
-- Licence:     LGPL3
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: TODO
--
-- Memoisation, using hash-consing as a way to identify arguments.

{-# OPTIONS_GHC -fno-float-in -fno-full-laziness #-}

{-# LANGUAGE
    AllowAmbiguousTypes, DataKinds, DefaultSignatures, FlexibleContexts,
    LambdaCase, ScopedTypeVariables, TypeApplications, TypeFamilies
  #-}

module Data.HashCons.Memo
  (-- * Memo-suitable arguments
   MemoArg (..),
   -- * Memoising functions
   uncheckedMemo, memo,
   -- * Nested memoisation
   -- $nesting
   memo2, memo3, memo4)
where

import Data.HashCons
import Data.HashCons.MkWeak

import Data.Hashable (Hashable)
import qualified Data.HashTable.IO as HashTable

import Data.Kind  (Type)

import Control.Concurrent.MVar
import System.IO.Unsafe


type HashTable k v = HashTable.BasicHashTable k v


-- | Types which can be arguments to a memo function. An empty instance assumes
-- that a type is its own key, and can't run finalizers. The latter is the case
-- for ordinary Haskell datatypes.
--
-- (TODO: add instances for everything in @base@)
class (Eq (Key k), Hashable (Key k)) => MemoArg k where
  -- | A key which uniquely identifies a value. Defaults to the value itself.
  -- Otherwise, it should be something with fast equality and hashing, and must
  -- really be a /unique/ identifier.
  type Key k :: Type
  type Key k = k

  -- | Extract the key. Defaults to the identity function for where
  -- @'Key' k ~ k@.
  key :: k -> Key k
  default key :: (Key k ~ k) => k -> Key k
  key x = x

  -- | Whether @k@ can reliably run finalizers. (Most datatypes can't; see the
  -- documentation for 'Weak' for details.)
  type CanFinalize k :: Bool
  type CanFinalize k = 'False

  -- | Add a finalizer, if possible; otherwise, do nothing. Defaults to
  -- doing nothing, for when @'CanFinalize' k ~ ''False'@.
  tryAddFinalizer :: k -> Finalizer -> IO ()
  tryAddFinalizer _ _ = pure ()


instance MemoArg Bool
instance MemoArg Ordering
instance MemoArg Char
instance MemoArg Int
instance MemoArg Integer
instance MemoArg Float
instance MemoArg Double
instance MemoArg Word
instance MemoArg ()

instance MemoArg a => MemoArg [a] where
  type Key [a] = [Key a]
  key = fmap key

instance MemoArg a => MemoArg (Maybe a) where
  type Key (Maybe a) = Maybe (Key a)
  key = fmap key

instance (MemoArg a, MemoArg b) => MemoArg (Either a b) where
  type Key (Either a b) = Either (Key a) (Key b)
  key = either (Left . key) (Right . key)


instance MemoArg (HC a) where
  type Key (HC a) = Tag a
  key = getTag
  type CanFinalize (HC a) = 'True
  tryAddFinalizer = addFinalizer


type MemoCache k v = MVar (HashTable (Key k) v)

newCache :: IO (MemoCache k v)
newCache = newMVar =<< HashTable.new


remove :: MemoArg k => Weak (MemoCache k v) -> Key k -> IO ()
remove wvar k =
  deRefWeak wvar >>= \case
    Nothing  -> pure ()
    Just var ->
      withMVar var $ \cache ->
        HashTable.delete cache k

lookupOrAdd :: MemoArg k => (k -> v) -> MemoCache k v -> k -> IO v
lookupOrAdd f var (x :: k) = do
  roCache <- readMVar var
  let !k = key x
  HashTable.lookup roCache k >>= \case
    Just y  -> pure y
    Nothing -> do
      let !y = f x
      withMVar var $ \rwCache ->
        HashTable.insert rwCache k y
      wvar <- mkWeakPtr var Nothing
      tryAddFinalizer x (remove @k wvar k)
      pure y


-- $nesting
-- It is possible to memoise a multiple-argument function by nesting calls to
-- 'memo' or 'uncheckedMemo', like so:
--
-- @
-- foo :: HC Beep -> HC Boop -> HC Bing -> HC Blah
--
-- memoFoo :: HC Beep -> HC Boop -> HC Bing -> HC Blah
-- memoFoo = memo $ \\x -> memo $ \\y -> memo $ foo x y
-- @
--
-- The functions 'memo2' to 'memo4' do this, with the first use being (checked)
-- 'memo' and the other(s) being 'uncheckedMemo'.
--
-- The user can use this pattern to write variations of a higher arity, or to
-- check whichever arguments are desired.
--
-- == Recommendations
--
-- * If possible, the first (or only) argument to a memoised function should be
--   able to run finalisers (e.g., @HC@): if a call to 'uncheckedMemo' is nested
--   inside a use of 'memo', then whole tables will be dropped by the outer
--   'memo''s finalizers when no longer needed, even though they might not
--   shrink before this time. Therefore, an outermost 'memo' ensures that the
--   memory usage is kept in check.
-- * If the least-long-lived arguments come first, then the pruning will be more
--   effective.

-- | Memoise a function, without checking that the memo table can be pruned. If
-- it can't, then it will continue to grow throughout the program's run.
uncheckedMemo :: MemoArg a => (a -> b) -> a -> b
uncheckedMemo (f :: a -> b) =
  let
    cache = unsafePerformIO (newCache @a)
    {-# NOINLINE cache #-}
  in
    \x -> unsafePerformIO $ lookupOrAdd f cache x

-- | Memoise a function, ensuring that the memo table can be pruned.
memo :: (MemoArg a, CanFinalize a ~ 'True) => (a -> b) -> a -> b
memo = uncheckedMemo

-- | Memoise a binary function, checking that the outer table can be pruned.
memo2 :: (MemoArg a, MemoArg b, CanFinalize a ~ 'True)
      => (a -> b -> c) -> a -> b -> c
memo2 f = memo $ \x -> uncheckedMemo $ f x

-- | Memoise a ternary function, checking that the outermost table can be
-- pruned.
memo3 :: (MemoArg a, MemoArg b, MemoArg c, CanFinalize a ~ 'True)
      => (a -> b -> c -> d) -> a -> b -> c -> d
memo3 f = memo $ \x -> uncheckedMemo $ \y -> uncheckedMemo $ f x y

-- | Memoise a quaternary function, checking that the outermost table can be
-- pruned.
memo4 :: (MemoArg a, MemoArg b, MemoArg c, MemoArg d, CanFinalize a ~ 'True)
      => (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
memo4 f =
  memo $ \x ->
    uncheckedMemo $ \y ->
      uncheckedMemo $ \z ->
        uncheckedMemo $ f x y z
