{-# OPTIONS_GHC -fplugin=Data.Record.Plugin.HasFieldPattern -ddump-parsed-ast -ddump-rn #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit
import qualified Unused
import TestTypes (REC(..))

-- kana: f :: HasField "a" Int r => HasField "b" Int r => r -> Int
-- kana: f ((\x -> (getField @"a" x, getField "b" x)) -> (a, b)) = a + b :: Int
f REC{a, b} = a + b :: Int

f0 REC{} = ()

f1 :: REC -> Int
f1 REC{a = p1} = p1
-- !(getField @"f1" -> p1)

f2 :: REC -> Int
f2 REC{a = p1, b = p2} = p1 + p1 :: Int
-- -!((\x -> (getField @"f1" x, getField @"f2")) -> (p1, p2))

f3 REC{a} = a

f4 REC{a, b} = a + b :: Int

f5 :: REC -> Int
f5 x = x.a + x.b

f6 :: REC -> Int
f6 x = x.a

f7 :: REC -> Int
f7 x = x.b

-- Actually typechecking is enough, but we do silly runtime test as well
hasFieldTests :: Assertion
hasFieldTests = do
  assertEqual "f1" 2 $ f1 $ REC{a=2, b = 3}
  assertEqual "f5" 5 $ f5 $ REC{a=2, b = 3}
  assertEqual "f6" 2 $ f6 $ REC{a=2, b = 3}
  assertEqual "f7" 3 $ f7 $ REC{a=2, b = 3}

main :: IO ()
main = defaultMain $ testGroup "large-anon" [
    testGroup "Sanity" [
      testCase "HasField" hasFieldTests
    ],
    Unused.unusedTests
  ]

