-- |
-- Module:      Data.HashCons.ConstRef
-- Description: Read-only references
-- Copyright:   © 2018 Andy Morris
-- Licence:     LGPL-3
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: GHC internals
--
-- Read-only references with pointer equality.

{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Data.HashCons.ConstRef
  (-- ** Basic API
   ConstRef, newConstRef, readConstRef,
   -- ** Weak pointers and finalization
   mkWeakConstRef, addConstRefFinalizer)
where

import GHC.Base  (mkWeak#, mkWeakNoFinalizer#)
import GHC.IO    (IO (..), unsafeDupablePerformIO)
import GHC.IORef (IORef (..), newIORef, readIORef)
import GHC.STRef (STRef (..))
import GHC.Weak  (Weak (..))


-- | A read-only reference.
--
-- A 'ConstRef' is similar to an 'IORef' in that it has its own identity.
--
-- @
-- do a <- 'newConstRef' ()
--    b <- 'newConstRef' ()
--    'return' '$' a '==' b
--      -- 'False'
-- @
--
-- However, unlike most types of reference, it is immutable and its value is set
-- at construction time. This is the reason why 'readConstRef' does not need to
-- return an 'IO' value.
newtype ConstRef a = CR (IORef a)
  deriving Eq -- ^ Pointer equality


-- | Make a new 'ConstRef'.
newConstRef :: a -> IO (ConstRef a)
newConstRef x =
  CR <$> newIORef x

-- | Read the value of a 'ConstRef'.
readConstRef :: ConstRef a -> a
readConstRef (CR r) =
  unsafeDupablePerformIO $ readIORef r


-- | Make a weak pointer with a 'ConstRef' as the key.
mkWeakConstRef :: ConstRef a -> b -> Maybe (IO ()) -> IO (Weak b)
mkWeakConstRef (CR (IORef (STRef r#))) v fin =
    IO $ \s1 -> case res s1 of (# s2, w #) -> (# s2, Weak w #)
  where
    res s = case fin of
      Just (IO fin#) -> mkWeak#            r# v fin# s
      Nothing        -> mkWeakNoFinalizer# r# v      s

-- | Add a finalizer to a 'ConstRef'.
addConstRefFinalizer :: ConstRef a -> IO () -> IO ()
addConstRefFinalizer r fin =
  () <$ mkWeakConstRef r () (Just fin)
