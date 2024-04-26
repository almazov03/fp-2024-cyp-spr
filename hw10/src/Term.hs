module Term where

data Term = Var String
          | App Term Term
          | Lam String Term
          deriving (Show, Eq)
