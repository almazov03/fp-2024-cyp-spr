module BaseNTest where

import Test.HUnit
import BaseN

baseNTests :: Test
baseNTests = TestList [
    TestCase $ assertEqual "show (BaseN 10 123)" "123_(10)" (show $ BaseN 10 123),
    TestCase $ assertEqual "show (BaseN 16 255)" "1515_(16)" (show $ BaseN 16 255),
    TestCase $ assertEqual "(BaseN 10 123) + (BaseN 10 456)" (BaseN 10 579) ((BaseN 10 123) + (BaseN 10 456)),
    TestCase $ assertEqual "(BaseN 10 123) - (BaseN 10 23)" (BaseN 10 100) ((BaseN 10 123) - (BaseN 10 23)),
    TestCase $ assertEqual "(BaseN 10 123) * (BaseN 10 4)" (BaseN 10 492) ((BaseN 10 123) * (BaseN 10 4))
  ]
