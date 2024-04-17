module Eval ( eval ) where 

import Expr ( Expr (..), BinOp (..), UnOp (..) )
import Error ( Error (..) )
import StateDemo

import qualified Data.Map.Strict as M

eval :: (Ord a, Floating a) => Expr a -> State (M.Map String a) (Either (Error a) a)
eval (Num d) = return $ Right d

eval (Var v) = do
    state <- get
    return $ case M.lookup v state of
        Just val -> Right val
        Nothing  -> Left $ VarNotDefined v

eval (UnOp Sqrt e) = do
    res <- eval e
    return $ either Left runSqrt res
  where 
    runSqrt v | v < 0 = Left (NegativeSqrt e)
              | otherwise = Right $ sqrt v

eval (BinOp op x y) = do
    x' <- eval x
    y' <- eval y
    return $ either Left (\xv -> either Left (runBinOp xv) y') x'
    where
        runBinOp xv yv = case op of
            Plus  -> Right $ xv + yv
            Mult  -> Right $ xv * yv
            Minus -> Right $ xv - yv
            Pow   -> Right $ xv ** yv
            Div | yv == 0   -> Left  $ DivisionByZero y
                | otherwise -> Right $ xv / yv