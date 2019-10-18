data Expr = C Float 
          | Expr :+ Expr 
          | Expr :- Expr 
          | Expr :* Expr 
          | Expr :/ Expr
          | V String 
          | Let [(String, Expr)] Expr
  deriving Show

subst :: [(String, Expr)] -> Expr -> Expr
subst vs (V v1) = 
  case lookup v1 vs of
    Nothing -> (V v1)
    Just e0 -> e0
subst _ (C c) = (C c)
subst vs (e1 :+ e2) = subst vs e1 :+ subst vs e2
subst vs (e1 :- e2) = subst vs e1 :- subst vs e2
subst vs (e1 :* e2) = subst vs e1 :* subst vs e2
subst vs (e1 :/ e2) = subst vs e1 :/ subst vs e2
subst vs (Let vs2 e2) = Let vs (subst vs2 e2)

evaluate :: Expr -> Float
evaluate (C x) = x
evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 / evaluate e2
evaluate (Let vs e1) = evaluate (subst vs e1)

data ExprCombinator a = ExprCombinator { constant :: Float -> a
                                       , plus :: a -> a -> a
                                       , minus :: a -> a -> a
                                       , mul ::  a -> a -> a
                                       , divide ::  a -> a -> a
                                       , var :: String -> a
                                       , bind :: ([(String, Expr)], Expr) -> a
                                       }

foldExpr cmb (C x) = constant cmb x
foldExpr cmb (e1 :+ e2) = (plus cmb) (foldExpr cmb e1) (foldExpr cmb e2)
foldExpr cmb (e1 :- e2) = (minus cmb) (foldExpr cmb e1) (foldExpr cmb e2)
foldExpr cmb (e1 :* e2) = (mul cmb) (foldExpr cmb e1) (foldExpr cmb e2)
foldExpr cmb (e1 :/ e2) = (divide cmb) (foldExpr cmb e1) (foldExpr cmb e2)
foldExpr cmb (V v) = var cmb v
foldExpr cmb (Let vs e1) = bind cmb (vs, e1)

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

substitueWithFold :: ([(String, Expr)], Expr) -> Expr
substitueWithFold (vs, e1) = foldExpr cmb e1 
  where 
    cmb = ExprCombinator { constant = C
                         , plus = (:+)
                         , minus = (:-)
                         , mul = (:*)
                         , divide = (:/)
                         , var = sub
                         , bind = (Let vs) . substitueWithFold
                         }
    sub v1 = case lookup v1 vs of
      Nothing -> (V v1)
      Just e0 -> e0

x = Let [("x", C 2), ("y", C 3)] $ 
  V "x" :* V "y"