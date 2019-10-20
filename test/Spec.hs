{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Main (main) where

import Data.Aeson as A

import GenericIllusion

import GHC.Generics

import Test.Tasty
import Test.Tasty.HUnit

data A = A
  { field1 :: Int
  , field2 :: String
  } deriving (Generic)

data B = B
  { field3 :: Int
  , field4 :: String
  } deriving (Generic)

newtype AB = AB (MergeRecords A B)
  deriving (Generic) via (MergeRecords A B)
  deriving (A.ToJSON, A.FromJSON)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [recordTests]

recordTests :: TestTree
recordTests = testCase "JSON Merge" $
  A.toJSON (AB $ MR (A 1 "2") (B 3 "4")) @?= A.object
    [ "field1" .= (1 :: Int)
    , "field2" .= ("2" :: String)
    , "field3" .= (3 :: Int)
    , "field4" .= ("4" :: String)
    ]
