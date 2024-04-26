import Test.Tasty
import Test.Tasty.HUnit

import Term
import Parser
import Lib

testLambdaParsing :: TestTree
testLambdaParsing = testGroup "Lambda Calculus Parser Tests"
  [ testCase "Parsing simple variable" $
      parseLambda "x" @?= Right (Var "x")

  , testCase "Parsing simple application" $
      parseLambda "x y" @?= Right (App (Var "x") (Var "y"))

  , testCase "Parsing nested applications" $
      parseLambda "x y z" @?= Right (App (App (Var "x") (Var "y")) (Var "z"))

  , testCase "Parsing lambda abstraction" $
      parseLambda "\\x.x" @?= Right (Lam "x" (Var "x"))

  , testCase "Parsing lambda with application" $
      parseLambda "\\x.x y" @?= Right (Lam "x" (App (Var "x") (Var "y")))

  , testCase "Parsing nested lambda" $
      parseLambda "\\x.\\y.x y" @?= Right (Lam "x" (Lam "y" (App (Var "x") (Var "y"))))

  , testCase "Parsing complex expression" $
      parseLambda "(\\x.x y) z" @?= Right (App (Lam "x" (App (Var "x") (Var "y"))) (Var "z"))

  , testCase "Parsing application of multiple lambdas" $
      parseLambda "(\\x.x) (\\y.y)" @?= Right (App (Lam "x" (Var "x")) (Lam "y" (Var "y")))

  , testCase "Handling unnecessary parentheses" $
      parseLambda "(x) (y)" @?= Right (App (Var "x") (Var "y"))

  , testCase "Parsing deeply nested applications" $
      parseLambda "x y z w" @?= Right (App (App (App (Var "x") (Var "y")) (Var "z")) (Var "w"))

  , testCase "Parsing mixed lambda and applications" $
      parseLambda "\\x.x y z" @?= Right (Lam "x" (App (App (Var "x") (Var "y")) (Var "z")))
  ]

testReductions :: TestTree
testReductions = testGroup "Lambda Calculus Reduction Tests"
  [ testCase "Normal-order reduction on simple lambda" $
      reduceNormal (App (Lam "x" (Var "x")) (Var "y")) @?= Var "y"

  , testCase "Applicative-order reduction on simple lambda" $
      reduceApplicative (App (Lam "x" (Var "x")) (Var "y")) @?= Var "y"

  , testCase "Normal-order reduction with nested applications" $
      reduceNormal (App (App (Lam "x" (Lam "y" (Var "x"))) (Var "a")) (Var "b")) @?= Var "a"

  , testCase "Applicative-order reduction with nested applications" $
      reduceApplicative (App (App (Lam "x" (Lam "y" (Var "x"))) (Var "a")) (Var "b")) @?= Var "a"

  , testCase "Normal-order reductions" $
      reduceNormal (App (Lam "x" (App (Var "x") (Var "x"))) (Lam "x" (Var "x"))) @?= Lam "x" (Var "x")

  , testCase "Applicative-order reductions" $
      reduceApplicative (App (Lam "x" (App (Var "x") (Var "x"))) (Lam "x" (Var "x"))) @?= Lam "x" (Var "x")
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ testLambdaParsing
  , testReductions
  ]