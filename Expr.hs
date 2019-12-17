module Expr where
    import Test.QuickCheck
    import Data.Char
    import Data.Maybe

    -- | An expression consists of: 
    -- | Numbers (integers of floating point)
    -- | Variables (x)
    -- | Operators (+, *)
    -- | Funtions (sin, cos)
    data Expr = Num Double
              | Var
              | Add Expr Expr
              | Mul Expr Expr
              | Sin Expr
              | Cos Expr
              deriving (Eq)

    instance Show Expr where
        show = showExpr

    ex1 = Add (Mul (Num 3) (Sin Var)) (Mul (Num 0.3) (Cos (Mul (Num 28) Var)))

    x :: Expr
    x = Var

    num :: Double -> Expr
    num = Num

    add,mul :: Expr -> Expr -> Expr
    add = Add
    mul = Mul

    sin,cos :: Expr -> Expr
    sin = Sin
    cos = Cos

    showExpr :: Expr -> String
    showExpr (Num n) = show n
    showExpr Var = "x"
    showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
    showExpr (Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2
    showExpr (Sin e) = "sin(" ++ showExpr e ++ ")"
    showExpr (Cos e) = "cos(" ++ showExpr e ++ ")"
    
    showFactor :: Expr -> String
    showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
    showFactor e = showExpr e

    eval :: Expr -> Double -> Double
    eval (Num n) _ = n
    eval Var x = x
    eval (Add e1 e2) x = eval e1 x + eval e2 x
    eval (Mul e1 e2) x = eval e1 x * eval e2 x
    eval (Sin e) x = Prelude.sin (eval e x)
    eval (Cos e) x = Prelude.cos (eval e x)

    