{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE
    DeriveDataTypeable, DeriveGeneric, PatternSynonyms, TemplateHaskell
  #-}

module TestHashCons (runTests) where

import Data.HashCons
import GHC.Generics
import Generics.SYB (Data, everything, mkQ)

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception (evaluate)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genString :: Gen String
genString = Gen.string (Range.linear 0 30) Gen.unicode

copyString :: String -> String
copyString = foldr (:) []


data HcString' =
    SNil'
  | SCons' Char HcString
  deriving (Eq, Show, Generic, Data)

type HcString = HC HcString'

pattern SNil :: HcString
pattern SNil = HC SNil'

pattern (:>) :: Char -> HcString -> HcString
pattern c :> cs = HC (SCons' c cs)

{-# COMPLETE SNil, (:>) #-}

instance NFData HcString'

instance Hashable HcString'
instance HashCons HcString'

fromString :: String -> HcString
fromString = foldr (:>) SNil

toString :: HcString -> String
toString SNil      = ""
toString (c :> cs) = c : toString cs

genHcString :: Gen HcString
genHcString = fromString <$> genString


genStringListWith :: String -> Gen (Int, [String])
genStringListWith str = do
  let listHalf = Gen.list (Range.linear 50 100) genString
  xs <- listHalf
  ys <- listHalf
  pure (length xs, xs ++ [str] ++ ys)


prop_hc_get_nonrec = property $ do
  a <- forAll genString
  getVal (hc a) === a

prop_eq_eq = property $ do
  a <- forAll genString
  let b = copyString a
  assert $ hc a == hc b

prop_rec = property $ do
  a <- forAll genHcString
  getVal a `deepseq` success

prop_neq_neq = property $ do
  a <- forAll genString
  b <- forAll $ Gen.filter (/= a) genString
  assert $ hc a /= hc b

prop_conc = property $ do
  str <- forAll genString
  (xi, xs) <- forAll $ genStringListWith str
  (yi, ys) <- forAll $ genStringListWith str
  let hcAll = evaluate . force . map fromString
  (xs', ys') <- evalIO $ concurrently (hcAll xs) (hcAll ys)
  assert $ xs'!!xi == ys'!!yi && toString (xs'!!xi) == str

prop_toString_everything = property $ do
  str <- forAll genHcString
  toString str === everything (++) (mkQ "" pure) str


runTests :: IO Bool
runTests = checkSequential $$(discover)
