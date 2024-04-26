module Main (main) where

import Parser
import Lib

main :: IO ()
main = do
    let expr = "(\\x. "
    case parseLambda expr of
        Left err -> print err
        Right ast -> do
            putStrLn "Parsed Term:"
            print $ prettyPrint ast
            putStrLn "Normal Order Reduction:"
            print $ prettyPrint $ reduceNormal ast
            putStrLn "Applicative Order Reduction:"
            print $ prettyPrint $ reduceApplicative ast