
import Expr
import Error
import Eval
import Test.Tasty
import Test.Tasty.HUnit
import StateDemo
import qualified Data.Map.Strict as M
import Control.Exception (assert)
import Parser



evalTests :: TestTree
evalTests = testGroup "Eval tests"
  [ testCase "Base test1" $ assertEqual "eval on number is number" (execState (eval (Num 777))M.empty) (Right 777)
  , testCase "Base test2" $ assertEqual "" (execState (eval (BinOp Pow (BinOp Div (BinOp Mult (BinOp Minus (BinOp Pow (BinOp Plus (UnOp Sqrt(Num 25)) (Num 3)) (Num 2)) (BinOp Mult (Num 4) (Num 7))) (Num 2)) (Num 5)) (Num 3)) )M.empty) (Right ( ((  ( (sqrt 25 + 3)**2 - 4 * 7) * 2) / 5)**3))
  , let e1 = BinOp Mult (Num 4) (Num 5)
        e2 = BinOp Plus (Num 1) (Num (-1)) in
    testCase "Zero division test" $ assertEqual "" (execState (eval (BinOp Div e1 e2)) M.empty) ( Left (DivisionByZero e2))
  , let e = BinOp Minus (Num 1) (Num 100) in
  
    testCase "RootOfNegative error" $ assertEqual "" (execState (eval (UnOp Sqrt e)) M.empty) (Left $ NegativeSqrt e)
  , testCase "Var test" $ assertEqual "" (execState (eval (Var "x")) $ M.fromList [("x", 100)]) (Right 100)

  
  , testCase "Undefined variable" $ assertEqual "" (execState (eval (BinOp Plus (Var "z") (Var "y"))) $ M.fromList [("x", 10), ("y", 20)]) ( Left $ VarNotDefined "z")
  , testCase "Base test 3" $ assertEqual "" (execState (eval (BinOp Minus (Var "x") (Var "x"))) $ M.fromList [("x", 0)]) (Right 0)
  , testCase "Pow test 0^0" $ assertEqual "" (execState (eval (BinOp Pow (Var "x") (Var "y"))) $ M.fromList [("x", 0), ("y", 0)]) (Right 1)
  ]



parserTests :: TestTree
parserTests = testGroup "Parser tests" [
  testCase "Integer" $ assertEqual "" (Just $ ("", Num 123)) (runParser parseExpr "123")
  , testCase "Integer with leading zeros" $ assertEqual "" (Just $ ("", Num 123)) (runParser parseExpr "00123")
  , testCase "Negative integer" $ assertEqual "" (Nothing) (runParser parseExpr "-123")
  , testCase "UnOp Sqrt test 1" $ assertEqual "" (Just $ ("", UnOp Sqrt $ Num 123)) (runParser parseExpr "sqrt 123")
  , testCase "UnOp Sqrt test 2" $ assertEqual "" (Just $ ("", UnOp Sqrt $ Var "xyz")) (runParser parseExpr "sqrt xyz")
  , testCase "BinOp test 1" $ assertEqual "" (Just $ ("", BinOp Plus (Num 123) (BinOp Mult (Num 45) (Num 6)))) (runParser parseExpr "+ 123 * 45 6")
  , testCase "BinOp test 2" $ assertEqual "" (Just $ ("", BinOp Plus (BinOp Mult (Num 123) (Num 45)) (Num 6))) (runParser parseExpr "+ * 123 45 6")
  ]

exprTests :: TestTree
exprTests = testGroup "Expr tests" [evalTests, parserTests]

main :: IO()
main = defaultMain exprTests