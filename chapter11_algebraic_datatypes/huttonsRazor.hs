data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit a) = a
eval (Add e1 e2) = (eval e1) + (eval e2)

-- Example of expected output:
-- Prelude> eval (Add (Lit 1) (Lit 9001))
-- 9002

printExpr :: Expr -> String
printExpr (Lit a) = show(a)
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2 

-- Expected output:
--     Prelude> printExpr (Add (Lit 1) (Lit 9001))
--     "1 + 9001"
