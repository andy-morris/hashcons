-- |
-- Module:      Data.HashCons.HC
-- Description: Hash-consed values
-- Copyright:   © 2018 Andy Morris
-- Licence:     LGPL-3
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: stable names
--
-- Hash-consed values.

module Data.HashCons.HC
  (HC, makeHC, getTag, getVal, Tag)
where

import Data.HashCons.HC.Internal
