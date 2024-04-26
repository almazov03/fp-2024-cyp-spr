module Lib where

import Term

prettyPrint :: Term -> String
prettyPrint (Var x) = x
prettyPrint (Lam x e) = "\\" ++ x ++ "." ++ prettyPrint e
prettyPrint (App e1 e2) = case e1 of
  Lam _ _ -> "(" ++ prettyPrint e1 ++ ") " ++ prettyPrint e2
  _       -> prettyPrint e1 ++ " " ++ prettyPrint e2

subst :: String -> Term -> Term -> Term
subst v n (Var x) = if x == v then n else Var x
subst v n (Lam x e) = if x == v then Lam x e else Lam x (subst v n e)
subst v n (App e1 e2) = App (subst v n e1) (subst v n e2)

reduceNormal :: Term -> Term
reduceNormal (Var x) = Var x
reduceNormal (Lam x e) = Lam x (reduceNormal e)
reduceNormal (App (Lam x e1) e2) = reduceNormal (subst x e2 e1)
reduceNormal (App e1 e2) = case reduceNormal e1 of
    Lam x e -> reduceNormal (App (Lam x e) e2)
    e1' -> App e1' e2

reduceApplicative :: Term -> Term
reduceApplicative (Var x) = Var x
reduceApplicative (Lam x e) = Lam x (reduceApplicative e)
reduceApplicative (App e1 e2) =
    let e1' = reduceApplicative e1
        e2' = reduceApplicative e2
    in case e1' of
        Lam x e -> reduceApplicative (subst x e2' e)
        _ -> App e1' e2'