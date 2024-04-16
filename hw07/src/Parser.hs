{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parser where 

import Data.Char ( isAlpha, isAlphaNum, isDigit, digitToInt, isSpace )
import Control.Applicative ( Alternative((<|>), empty, many, some) )
import Expr

keywords :: [String]
keywords = ["if", "then", "else"]

-- This is a straightforward implementation of an indetifier parser. 
parseIdent :: String -> Either String String 
parseIdent str@(h:t) 
    | isValidFirst h && all isValid t = 
        if str `notElem` keywords 
        then Right str 
        else Left "Keyword cannot be an identifier"
    | otherwise = Left "Lexical error: inadmissible character"
  where 
    isValidFirst x = x == '_' || x == '\'' || isAlpha x 
    isValid x = isAlphaNum x || isValidFirst x 
parseIdent [] = Left "Empty string is not an identifier"

-- This is a straightforward implementation of a number parser. 
parseInt :: String -> Either String Int 
parseInt (h:t) = 
    if h == '-'
    then ((-1) *) <$> parseNumber t 
    else parseNumber (h:t)
  where 
    parseNumber [] = Left "Empty string is not a number"
    parseNumber str = 
        if all isDigit str
        then Right $ toDigit str 
        else Left "Lexical error: inadmissible character"
      where 
        toDigit = foldl1 (\a x -> a * 10 + x) . map digitToInt
parseInt [] = Left "Empty string is not a number"

-- It's not clear how to compose the parsers above, so people usually use a different abstraction for a parser. 
-- A parser consumes the prefix of an input String while it's a valid string of the language being parsed. 
-- Then the unread suffix is returned along with the result of the parsing. 
-- The result may be a string (for identifiers), an integer (for numbers), 
-- some algebraic data type for more complex langugaes (for example, Expr for expressions), 
-- or even a function. 
newtype Parser a 
  = Parser { runParser :: String -> Maybe (String, a)}

-- This abstraction of a parser is a Functor, which allows us to transform the parser's results. 
instance Functor Parser where 
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input -> 
    case runParser p input of 
      Nothing -> Nothing 
      Just (suff, r) -> Just (suff, f r) 
      
-- The parser is also an applicative functor, which simplifies composition.       
instance Applicative Parser where
  pure :: a -> Parser a
  pure res = Parser $ \str -> Just (str, res)
  
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) f p = Parser $ \str ->
    case runParser f str of
      Just (str', f') ->
        case runParser p str' of
          Just (str'', a) -> Just (str'', f' a)
          Nothing -> Nothing
      Nothing -> Nothing
    
-- Monadic bind is something which expresses the idea of sequential parser application. 
-- First parse the string with this parser, and then parse the rest with that parser.  
-- This is one of two most important operations on parsers.    
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) f p = Parser $ \str ->
    case runParser f str of
      Just (str', res) -> runParser (p res) str'
      Nothing -> Nothing

-- Alternative operation allows us to express that something is either this or that. 
-- Note that it favors the left-hand parser: if it succeeds consuming any prefix of the input string, 
-- the right parser will not be tried. 
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing -- a parser which always reports an error: no strings in its language.

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) l r = Parser $ \str ->
    case runParser l str of
      Just (str', res) -> Just (str', res)
      Nothing -> runParser r str

-- This function creates a parser which checks that a predicate holds for the first character of an input string.  
satisfy :: (Char -> Bool) -> Parser Char 
satisfy p = Parser $ \str -> 
  case str of 
    (h:t) | p h -> Just (t, h) 
    _ -> Nothing

parseInteger :: Num a => Parser (Expr a)
parseInteger = do
    digits <- some $ satisfy isDigit
    return $ fromInteger $ toInteger (foldl (\n d -> n*10 + d) 0 (map digitToInt digits))

parseIdentifier :: Parser String
parseIdentifier = do
    h <- satisfy isAlpha
    t <- many $ satisfy isAlphaNum
    return (h:t)

parseVariable :: Parser (Expr a)
parseVariable = do
    word <- parseIdentifier
    if elem word keywords
        then Parser $ \_ -> Nothing
        else return $ Var word

parseSqrt :: Num a => Parser (Expr a)
parseSqrt = do
    ident <- parseIdentifier
    case ident of
        "sqrt" -> do
            some $ satisfy isSpace
            innerExpr <- parseExpr
            return $ UnOp Sqrt innerExpr
        _ -> Parser $ \_ -> Nothing

parseBinaryOp  :: Num a => Parser (Expr a)
parseBinaryOp = do
    op <- binopChar
    some $ satisfy isSpace
    e1 <- parseExpr
    some $ satisfy isSpace
    e2 <- parseExpr
    return $ op e1 e2
    where
        binopChar = do
            opChar <- satisfy $ \c -> c `elem` "+-*/^"
            return (case opChar of
                '+' -> BinOp Plus
                '-' -> BinOp Minus
                '*' -> BinOp Mult
                '/' -> BinOp Div
                '^' -> BinOp Pow)

parseExpr :: Num a =>  Parser (Expr a)
parseExpr = parseInteger <|> parseSqrt <|> parseVariable <|> parseBinaryOp