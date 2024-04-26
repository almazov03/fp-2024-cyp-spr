module Parser where

import Term
import Text.Parsec
import Text.Parsec.String (Parser)

variable :: Parser Term
variable = Var <$> many1 letter <* spaces

lambda :: Parser Term
lambda = do
    char '\\' *> spaces
    v <- many1 letter <* spaces
    char '.' *> spaces
    Lam v <$> term

application :: Parser Term
application = foldl1 App <$> many1 (atom <* spaces)

atom :: Parser Term
atom = variable <|> lambda <|> parens term

parens :: Parser a -> Parser a
parens p = char '(' *> spaces *> p <* spaces <* char ')'

term :: Parser Term
term = spaces *> (lambda <|> application <|> atom) <* spaces

parseLambda :: String -> Either ParseError Term
parseLambda = parse term ""