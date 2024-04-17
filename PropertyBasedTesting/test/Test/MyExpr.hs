module Test.MyExpr where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Debug.Trace

import Expr as Expr
import StateDemo as StateDemo
import Eval as Eval
import qualified ExprParser as Parser
import qualified Data.Map.Strict as M

genBinop :: Gen BinOp
genBinop = Gen.element [Plus, Minus, Mult, Div, Pow]

genExpr :: Int -> Int -> Gen (Expr Int)
genExpr n varLength =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      numGen
      , varGen
    ]
    [ -- recursive generators
      binOpGen
      , unOpGen 
    ]
  where
    varGen = Expr.Var <$> Gen.string (Range.constant 1 varLength) Gen.alpha
    numGen = Expr.Num <$> Gen.int (Range.constant 1 n)
    unOpGen = Gen.subterm (genExpr n varLength) (UnOp Sqrt)
    binOpGen = do
      op <- genBinop
      Gen.subterm2 (genExpr n varLength) (genExpr n varLength) (BinOp op)
  
genExprDouble :: Int -> Gen (Expr Double)
genExprDouble varLength =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      numGen
      , varGen
    ]
    [ -- recursive generators
      binOpGen
    ]
  where
    varGen = Expr.Var <$> Gen.string (Range.constant 1 varLength) Gen.alpha
    numGen = Expr.Num <$> Gen.double (Range.constant 1.0 10.0)
    unOpGen = Gen.subterm (genExprDouble varLength) (UnOp Sqrt)
    binOpGen = do
      op <- genBinop
      Gen.subterm2 (genExprDouble varLength) (genExprDouble varLength) (BinOp op)


-- parser . printer == id
parserPrinterIsId :: MonadTest m => (Expr Int -> String) -> (String -> Maybe (String, Expr Int)) -> Expr Int -> m ()
parserPrinterIsId printer parser ast =
  case parser (printer ast) of
    Just ("", r) -> trace (show ast)(r === ast)
    _ -> trace (show ast) failure

prop_printerParserPrefix :: Property
prop_printerParserPrefix = property $ do
  expr <- forAll $ genExpr 100 1
  parserPrinterIsId show (Parser.runParser Parser.parseExpr) expr

prop_divisionSelfIsOne :: Property
prop_divisionSelfIsOne = property $ do
  expr <- forAll $ genExprDouble 1
  let result = execState (Eval.eval (BinOp Div expr expr)) M.empty
  case result of
    Right val ->
      if val == 1
        then success 
        else failure
    Left err -> success

prop_multOne :: Property
prop_multOne = property $ do
  expr <- forAll $ genExprDouble 100
  let result1 = execState (Eval.eval (BinOp Mult expr 1)) M.empty
  let result2 = execState (Eval.eval (BinOp Mult 1 expr)) M.empty
  if result1 == result2 && result1 == execState (Eval.eval expr) M.empty then success
  else failure

prop_multZero :: Property
prop_multZero = property $ do
  expr <- forAll $ genExprDouble 100
  let result1 = execState (Eval.eval (BinOp Mult expr 0)) M.empty
  let result2 = execState (Eval.eval (BinOp Mult 0 expr)) M.empty
  case (result1, result2) of
    (Right v1, Right v2) ->   if (result1 == result2) && (result1 == execState (Eval.eval (Num 0)) M.empty) then success
                              else failure
    _ -> success

props :: [TestTree]
props =
  [
    testProperty "`parser . printer == id`" prop_printerParserPrefix
    , testProperty "e/e=1"  prop_divisionSelfIsOne
    , testProperty "e*1 = 1*e = e"  prop_multOne
    , testProperty "e*0 = 0*e = 0"  prop_multZero
  ]
