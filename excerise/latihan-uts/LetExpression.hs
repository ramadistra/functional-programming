data Expr = C Float 
          | Expr :+ Expr 
          | Expr :- Expr 
          | Expr :* Expr 
          | Expr :/ Expr
          | V String 
          | Let String Expr Expr
  deriving Show

subst :: String -> Expr -> Expr -> Expr
subst v0 e0 (V v1) = if (v0 == v1) then e0 else (V v1)
subst _ _ (C c) = (C c)
subst v0 e0 (e1 :+ e2) = subst v0 e0 e1 :+ subst v0 e0 e2
subst v0 e0 (e1 :- e2) = subst v0 e0 e1 :- subst v0 e0 e2
subst v0 e0 (e1 :* e2) = subst v0 e0 e1 :* subst v0 e0 e2
subst v0 e0 (e1 :/ e2) = subst v0 e0 e1 :/ subst v0 e0 e2
subst v0 e0 (Let v1 e1 e2) = Let v0 e0 (subst v1 e1 e2)

evaluate :: Expr -> Float
evaluate (C x) = x
evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 / evaluate e2
evaluate (Let v e0 e1) = evaluate (subst v e0 e1)

data ExprCombinator a = ExprCombinator { constant :: Float -> a
                                       , plus :: a -> a -> a
                                       , minus :: a -> a -> a
                                       , mul ::  a -> a -> a
                                       , divide ::  a -> a -> a
                                       , var :: String -> a
                                       , bind :: (String, Expr, Expr) -> a
                                       }

foldExpr cmb (C x) = constant cmb x
foldExpr cmb (e1 :+ e2) = (plus cmb) (foldExpr cmb e1) (foldExpr cmb e2)
foldExpr cmb (e1 :- e2) = (minus cmb) (foldExpr cmb e1) (foldExpr cmb e2)
foldExpr cmb (e1 :* e2) = (mul cmb) (foldExpr cmb e1) (foldExpr cmb e2)
foldExpr cmb (e1 :/ e2) = (divide cmb) (foldExpr cmb e1) (foldExpr cmb e2)
foldExpr cmb (V v) = var cmb v
foldExpr cmb (Let v e0 e1) = bind cmb (v, e0, e1)

evalWithFold :: Expr -> Float
evalWithFold = foldExpr $ 
  ExprCombinator { constant = id
                 , plus = (+)
                 , minus = (-)
                 , mul = (*)
                 , divide = (/)
                 , var = undefined
                 , bind = evalWithFold . substitueWithFold
                 }

substitueWithFold :: (String, Expr, Expr) -> Expr
substitueWithFold (v0, e0, e1) = foldExpr cmb e1 
  where 
    cmb = ExprCombinator { constant = C
                         , plus = (:+)
                         , minus = (:-)
                         , mul = (:*)
                         , divide = (:/)
                         , var = sub
                         , bind = (Let v0 e0) . substitueWithFold
                         }
    sub v1 = if (v0 == v1) then e0 else (V v1)

countConstants :: Expr -> Int
countConstants = foldExpr $ 
  ExprCombinator { constant = \x -> 1
                 , plus = (+)
                 , minus = (+)
                 , mul = (+)
                 , divide = (+)
                 , var = \x -> 0
                 , bind = \(_, e0, e1) -> countConstants e0 + countConstants e1
                 }

x = Let "x" (C 2) $
      Let "y" (C 3) $ 
        Let "x" (V "y") $
          V "x" :* V "y"