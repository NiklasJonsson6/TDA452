module Expr where
    import Test.QuickCheck
    import Data.Char
    import Data.Maybe
    import Parsing
    import System.Random

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

    readExpr :: String -> Maybe Expr
    readExpr s = case parse exprP (trim s) of 
        Just (e,_) -> Just e
        _ -> Nothing
        where trim = filter (' ' /=)

    numP,exprP,termP,factorP,varP,sinP,cosP :: Parser Expr
    numP = Num <$> readsP 
    exprP = foldr1 Add <$> chain termP (char '+')
    termP = foldr1 Mul <$> chain factorP (char '*')
    factorP = char '(' *> exprP <* char ')' <|> numP <|> varP <|> sinP <|> cosP
    varP = char 'x' *> return Var
    sinP = Sin <$> (char 's' *> char 'i' *> char 'n' *> factorP) 
    cosP = Cos <$> (char 'c' *> char 'o' *> char 's' *> factorP)

    prop_ShowReadExpr e = Just (assoc e) == readExpr (show e)

    instance Arbitrary Expr where
        arbitrary = sized rExpr


    range = 4
    rExpr :: Int -> Gen Expr
    rExpr s = frequency [
        (1, rNum),
        (1, return Var),
        (s, do a <- rExpr s'
               b <- rExpr s'
               return (Add a b)),
        (s, do a <- rExpr s'
               b <- rExpr s'
               return (Mul a b)),
        (s, do a <- rExpr s'
               return (Sin a)),
        (s, do a <- rExpr s'
               return (Cos a))
        ]
        where 
            s' = div s 2
            rNum = elements (map Num [-range..range])

    assoc :: Expr -> Expr
    assoc (Add (Add a b) c) = assoc (Add a (Add b c))
    assoc (Add a b)         = Add (assoc a) (assoc b)
    assoc (Mul (Mul a b) c) = assoc (Mul a (Mul b c))
    assoc (Mul a b)         = Mul (assoc a) (assoc b)
    assoc (Sin a)           = Sin (assoc a)
    assoc (Cos a)           = Cos (assoc a)
    assoc a                 = a