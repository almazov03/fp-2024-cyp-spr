module RotateTest (rotateTests) where

import Test.HUnit
import Rotate

rotateTests :: Test
rotateTests = TestList [
    TestCase $ assertEqual "rotate 2 \"abcdef\"" "cdefab" (rotate 2 "abcdef"),
    TestCase $ assertEqual "rotate -2 \"abcdef\"" "efabcd" (rotate (-2) "abcdef"),
    TestCase $ assertEqual "rotate 0 \"abcdef\"" "abcdef" (rotate 0 "abcdef"),
    TestCase $ assertEqual "rotate 6 \"abcdef\"" "abcdef" (rotate 6 "abcdef"),
    TestCase $ assertEqual "rotate 8 \"abcdef\"" "cdefab" (rotate 8 "abcdef"),
    TestCase $ assertEqual "rotate (-8) \"abcdef\"" "efabcd" (rotate (-8) "abcdef")
  ]
