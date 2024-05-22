module Main where

import Test.HUnit
import BaseNTest
import RotateTest
import TreeTest

main :: IO ()
main = do
  _ <- runTestTT $ TestList [baseNTests, rotateTests, treeTests]
  return ()

