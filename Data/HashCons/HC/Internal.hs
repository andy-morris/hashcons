-- |
-- Module:      Data.HashCons.HC.Internal
-- Description: Hash-consed values
-- Copyright:   © 2018 Andy Morris
-- Licence:     LGPL-3
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: stable names
--
-- Hash-consed values (internals).

module Data.HashCons.HC.Internal where

import Data.HashCons.ConstRef
import Data.HashCons.MkWeak

import Data.Hashable

import Control.DeepSeq

import System.Mem.StableName


-- | A tag for a value. Tags are unique among values which are simultaneously
-- alive.
newtype Tag a = Tag {fromTag :: StableName a} deriving Eq

instance Hashable (Tag a) where
  hash         = hash . fromTag
  hashWithSalt = hashUsing fromTag

makeTag :: a -> IO (Tag a)
makeTag x = fmap Tag . makeStableName $! x


-- | A value which has been given a unique tag.
data HC a = HC {-# UNPACK #-} !(Tag a) !(ConstRef a)

-- | Make an @HC@ value.
makeHC :: a -> IO (HC a)
makeHC x = HC <$> makeTag x <*> newConstRef x

getTag :: HC a -> Tag a
getTag (HC t _) = t

getVal :: HC a -> a
getVal (HC _ x) = readConstRef x


-- | \(\mathcal{O}(1)\) using the tag
instance Eq (HC a) where
  x == y = getTag x == getTag y

-- | Checks the tag for equality first, and otherwise falls back to the
-- underlying type's ordering
instance Ord a => Ord (HC a) where
  compare x y = if x == y then EQ else compare (getVal x) (getVal y)

-- | Shows the underlying value
instance Show a => Show (HC a) where
  showsPrec d = showsPrec d . getVal

-- | \(\mathcal{O}(1)\) using the tag
instance Hashable (HC a) where
  hash         = hash . getTag
  hashWithSalt = hashUsing getTag

-- | Also evaluates the underlying value
instance NFData a => NFData (HC a) where
  rnf = rnf . getVal

instance MkWeak (HC a) where
  mkWeak (HC _ x) = mkWeak x
