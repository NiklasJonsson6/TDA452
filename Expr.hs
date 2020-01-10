module Expr where
import Parsing
import Prelude hiding (cos,sin)
import qualified Prelude as P (cos, sin)
import Data.Char
import Test.QuickCheck
import Data.Maybe

data Expr = Num Double
          | Var 
          | Func FunOp Expr
          | Opr BinOp Expr Expr
          deriving(Eq, Show)
data FunOp = Sin | Cos
        deriving(Eq, Show)
data BinOp = Add | Mul
        deriving(Eq, Show)

x :: Expr
x = Var

num :: Double -> Expr
num = Num 

add,mul :: Expr -> Expr -> Expr
add = Opr Add
mul = Opr Mul

sin,cos :: Expr -> Expr
sin = Func Sin
cos = Func Cos

showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr Var = "x"
showExpr (Opr Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Opr Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2
showExpr (Func Sin e) = "sin(" ++ showExpr e ++ ")"
showExpr (Func Cos e) = "cos(" ++ showExpr e ++ ")"

showFactor :: Expr -> String
showFactor (Opr Add e1 e2) = "(" ++ showExpr (Opr Add e1 e2) ++ ")"
showFactor e = showExpr e


eval :: Expr -> Double -> Double
eval (Num n) d   = n
eval  Var    d   = d
eval (Func Cos a) d   = P.cos (eval a d) 
eval (Func Sin a) d   = P.sin (eval a d) 
eval (Opr Add a b) d = eval a d + eval b d
eval (Opr Mul a b) d = eval a d * eval b d

double :: Parser Expr
double =  Num <$> readsP 

variable :: Parser Expr
variable = do 
        char 'x' 
        return x

cosP :: Parser Expr
cosP = do 
        char 'c'
        char 'o'
        char 's'
        cos <$> factor

sinP :: Parser Expr
sinP = do 
        char 's'
        char 'i'
        char 'n'
        sin <$> factor

factor :: Parser Expr
factor = cosP <|> sinP <|> variable <|> double <|> char '(' *> expr <* char ')'
term :: Parser Expr
term = foldl1 (Opr Mul) <$> chain factor (char '*')
expr :: Parser Expr
expr   = foldl1 (Opr Add) <$> chain term (char '+')

calculation :: Parser Expr
calculation = term<|>expr

readExpr :: String -> Maybe Expr
readExpr s =  do
                let s' = filter (not . isSpace) s    
                case parse expr s' of       
                     Just (e,_) -> Just e     
                     _ -> Nothing      

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e =  appr x y
             where e' = fromJust (readExpr(showExpr e))
                   x = eval e 1.0
                   y = eval e' 1.0
                   appr a b = abs(a-b) < 1.0e-6

range = 4
arbExpr :: Int -> Gen Expr
arbExpr s = frequency [
    (1, rNum),
    (1, return Var),
    (s, do  a <- arbExpr s'
            b <- arbExpr s'
            return (Opr Add a b)),
    (s, do  a <- arbExpr s'
            b <- arbExpr s'
            return (Opr Mul a b)),
    (s, do  a <- arbExpr s'
            return (Func Sin a)),
    (s, do  a <- arbExpr s'
            return (Func Cos a))
    ]
    where 
        s' = div s 2
        rNum = elements (map Num [-range..range])

instance Arbitrary Expr where
        arbitrary = sized arbExpr


simplify :: Expr -> Expr
simplify (Opr Add a b)  =  simpAdd (simplify a) (simplify b)
simplify (Opr Mul a b)  =  simpMul (simplify a) (simplify b)
simplify (Func f a )  =  simpFunc f a
simplify  e         =   e

simpFunc :: FunOp -> Expr -> Expr
simpFunc f (Num n) = Num (eval (Func f (Num n)) n)
simpFunc f e = Func f (simplify e)

simpAdd :: Expr -> Expr -> Expr
simpAdd (Num 0)          b            = simplify b
simpAdd  a              (Num 0)       = simplify a
simpAdd (Num a)         (Num b)       = Num (a+b)
simpAdd  Var             Var          = Opr Mul (Num 2) Var
simpAdd (Opr Mul (Num a) b) (Opr Mul c (Num d)) | showExpr b == showExpr c = Opr Mul (Num (a+d)) b
simpAdd  a              (Opr Mul b (Num c)) | showExpr a == showExpr b = Opr Mul (Num (c+1)) a
simpAdd (Opr Mul (Num a) b)  c              | showExpr b == showExpr c = Opr Mul (Num (a+1)) b
simpAdd  a               b            = Opr Add a b 


simpMul :: Expr -> Expr -> Expr
simpMul (Num 0)          b              = Num 0
simpMul  a              (Num 0)         = Num 0
simpMul (Num 1)          b              = b
simpMul  a              (Num 1)         = a
simpMul (Num a)         (Num b)         = Num (a*b)
simpMul (Opr Mul (Num a) b) (Num c)         = Opr Mul (Num (a*c)) b
simpMul (Num a)         (Opr Mul b (Num c)) = Opr Mul (Num (a*c)) b
simpMul  a               b              = Opr Mul a b 

prop_simplify :: Expr -> Bool
prop_simplify e = appr (eval e 1.0) (eval e' 1.0)
           where    e' = simplify e
                    appr a b = abs(a-b) < 1.0e-6

differentiate :: Expr -> Expr
differentiate (Num n)   = Num 0
differentiate  Var      = Num 1
differentiate (Opr Add a b) = simplify $ Opr Add (differentiate a) (differentiate b)
differentiate (Opr Mul a b) = simplify $ Opr Add (Opr Mul (differentiate b) a) (Opr Mul (differentiate a) b)
differentiate (Func Sin a)   = simplify $ Opr Mul (Func Cos a)           (differentiate a)
differentiate (Func Cos a)   = simplify $ Opr Mul (Num (-1)) (Opr Mul (differentiate a) (Func Sin a))