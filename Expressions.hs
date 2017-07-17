module Expressions where
import MoreTrig
import Data.List

infixr 5 :+:
infixr 5 :-:
infixr 7 :*:
infixr 7 :/:
infixl 8 :^:

data Expr a = Var String
    | Const a
    | (Expr a) :+: (Expr a)
    | (Expr a) :-: (Expr a)
    | (Expr a) :*: (Expr a)
    | (Expr a) :/: (Expr a)
    | (Expr a) :^: (Expr a)
    | Parens (Expr a)
    | Abs (Expr a)
    | Exp (Expr a)
    | Log (Expr a)
    | Sin (Expr a)
    | Cos (Expr a)
    | Tan (Expr a)
    | Sec (Expr a)
    | Csc (Expr a)
    | Cot (Expr a)
    | Asin (Expr a)
    | Acos (Expr a)
    | Atan (Expr a)
    | Asec (Expr a)
    | Acsc (Expr a)
    | Acot (Expr a)
    | Sinh (Expr a)
    | Cosh (Expr a)
    | Tanh (Expr a)
    | Sech (Expr a)
    | Csch (Expr a)
    | Coth (Expr a)
    | Asinh (Expr a)
    | Acosh (Expr a)
    | Atanh (Expr a)
    | Asech (Expr a)
    | Acsch (Expr a)
    | Acoth (Expr a)
    | Sqrt (Expr a)
    | Sign (Expr a)

instance (Eq m,Num m) => Eq ( Expr m) where
    Const a == Const b = a == b
    Var a == Var b = a == b
    Const _ == Var _ = False
    Var _ == Const _ = False
    Parens a == Parens b = a == b
    Parens b == a = a == b
    a == Parens b = a == b
    Abs a == Abs b = (a == b) || (a == Const (-1) :*: b) || (Const (-1) :*: a == b)
    Exp a == Exp b = a == b
    Log a == Log b = a == b
    Sin a == Sin b = a == b
    Cos a == Cos b = a == b
    Tan a == Tan b = a == b
    Sec a == Sec b = a == b
    Csc a == Csc b = a == b
    Cot a == Cot b = a == b
    Asin a == Asin b = a == b
    Acos a == Acos b = a == b
    Atan a == Atan b = a == b
    Asec a == Asec b = a == b
    Acsc a == Acsc b = a == b
    Acot a == Acot b = a == b
    Sinh a == Sinh b = a == b
    Cosh a == Cosh b = a == b
    Tanh a == Tanh b = a == b
    Sech a == Sech b = a == b
    Csch a == Csch b = a == b
    Coth a == Coth b = a == b
    Asinh a == Asinh b = a == b
    Acosh a == Acosh b = a == b
    Atanh a == Atanh b = a == b
    Asech a == Asech b = a == b
    Acsch a == Acsch b = a == b
    Acoth a == Acoth b = a == b
    Sqrt a == Sqrt b = a == b
    Sign a == Sign b = a == b
    (a :+: b) == (c :+: d) = ((a == c) && (b == d)) || ((a == d) && (b == c))
    (a :*: b) == (c :*: d) = ((a == c) && (b == d)) || ((a == d) && (b == c))
    (a :-: b) == (c :-: d) = (a == c) && (b == d)
    (a :/: b) == (c :/: d) = (a == c) && (b == d)
    (a :^: b) == (c :^: d) = (a == c) && (b == d)
    _ == _ = False --Give Up!

instance ( Show m, Num m, Eq m) => Show (Expr m) where
    --Standard Mode
    {--}
    show (Const a)
        | otherwise     = show a
    show (Var a)     = a
    show (Const a :*: b)
        | a == (-1) = "-" ++ show(b)
        | otherwise = show(a) ++ show(b)
    show (b :*: Const a)
        | a == (-1) = "-" ++ show(b)
        | otherwise = show(a) ++ show(b)
    show (a :+: b)   = show a ++ " + " ++ show b
    show (a :*: b)   = show a ++ " * " ++ show b
    show (a :-: b)   = show a ++ " - " ++ show b
    show (a :/: b)   = show a ++ " / " ++ show b
    show (a :^: b)   = show a ++ " ^ " ++ show b
    show ( Parens a) = "( " ++ show a ++ " )"
    show ( Abs a)    = "| " ++ show a ++ " |"
    show ( Log a)    = "Log ( " ++ show a ++ " )"
    show ( Exp a)    = "e ^ ( " ++ show a ++ " )"
    show ( Sin a)    = "Sin ( " ++ show a ++ " )"
    show ( Cos a)    = "Cos ( " ++ show a ++ " )"
    show ( Tan a)    = "Tan ( " ++ show a ++ " )"
    show ( Sec a)    = "Sec ( " ++ show a ++ " )"
    show ( Csc a)    = "Csc ( " ++ show a ++ " )"
    show ( Cot a)    = "Cot ( " ++ show a ++ " )"
    show ( Asin a)   = "ASin ( " ++ show a ++ " )"
    show ( Acos a)   = "ACos ( " ++ show a ++ " )"
    show ( Atan a)   = "ATan ( " ++ show a ++ " )"
    show ( Asec a)    = "ASec ( " ++ show a ++ " )"
    show ( Acsc a)    = "ACsc ( " ++ show a ++ " )"
    show ( Acot a)    = "ACot ( " ++ show a ++ " )"
    show ( Sinh a)   = "Sinh ( " ++ show a ++ " )"
    show ( Cosh a)   = "Cosh ( " ++ show a ++ " )"
    show ( Tanh a)   = "Tanh ( " ++ show a ++ " )"
    show ( Sech a)    = "Sech ( " ++ show a ++ " )"
    show ( Csch a)    = "Csch ( " ++ show a ++ " )"
    show ( Coth a)    = "Coth ( " ++ show a ++ " )"
    show ( Asinh a)  = "ASinh ( " ++ show a ++ " )"
    show ( Acosh a)  = "Acosh ( " ++ show a ++ " )"
    show ( Atanh a)  = "Atanh ( " ++ show a ++ " )"
    show ( Asech a)    = "ASech ( " ++ show a ++ " )"
    show ( Acsch a)    = "ACsch ( " ++ show a ++ " )"
    show ( Acoth a)    = "ACoth ( " ++ show a ++ " )"
    show ( Sqrt a)   = "√( " ++ show a ++ " )"
    show ( Sign a)   = "Sign (" ++ show a ++ " )"
    {--}
instance (Num m, Eq m) => Num (Expr m) where
    a + b = a :+: b
    a * Const b = Const b :*: a --Makes Things Easier if Constants go first
    a * b
        | (getPrec b >= 7) && (getPrec a >= 7) = a :*: b
        | getPrec b >= 7 = Parens a :*: b
        | getPrec a >= 7 = a  :*: Parens b
        | otherwise = Parens a :*: Parens b
    a - b = a :-: b
    negate (Const a) = Const ( -1 * a)
    negate (Const (-1) :*: a) = a
    negate (a :*: Const (-1)) = a
    negate a = Const (-1) * a
    abs (Const a) = Const (abs a)
    abs (a :*: Const (-1)) = a
    abs (Const (-1) :*: a) = a
    abs a = Abs a
    signum (Const a) = Const (signum a)
    signum a = Sign a
    fromInteger a = Const (fromInteger a)
instance (Fractional m, Eq m) => Fractional (Expr m) where
    a / b
        | (getPrec b >= 7) && (getPrec a >= 7) = a :/: b
        | getPrec b >= 7 = Parens a :/: b
        | getPrec a >= 7 = a :/: Parens b
        | otherwise = Parens a :/: Parens b
    fromRational a = Const(fromRational a)
instance (Floating m, Eq m) => Floating (Expr m) where
    pi = Const pi
    exp = Exp
    log = Log
    sin = Sin
    cos = Cos
    tan = Tan
    asin = Asin
    acos = Acos
    atan = Atan
    sinh = Sinh
    cosh = Cosh
    tanh = Tanh
    asinh = Asinh
    acosh = Acosh
    atanh = Atanh
    sqrt = Sqrt
    a ** b
        | (getPrec b >= 8) && (getPrec a >= 8) = a :^: b
        | getPrec b >= 8 = Parens a :^: b
        | getPrec a >= 8 = a :^: Parens b
        | otherwise = Parens a :^: Parens b

-- Get the precedence of the outermost expression of a given operation
getPrec :: Expr a -> Integer
getPrec (_ :+: _) = 5
getPrec (_ :-: _) = 5
getPrec (_ :*: _) = 7
getPrec (_ :/: _) = 7
getPrec (_ :^: _) = 8
getPrec _ = 10

infixr 5 +|+
(+|+) a b = nub(a ++ b)

vars :: (Num a, Eq a) => Expr a -> [Expr a]
vars (Var a) = [Var a]
vars (Const a) = []
vars (a :+: b) = vars a +|+ vars b
vars (a :-: b) = vars a +|+ vars b
vars (a :*: b) = vars a +|+ vars b
vars (a :/: b) = vars a +|+ vars b
vars (a :^: b) = vars a +|+ vars b
vars (Parens a) = vars a
vars (Abs a)   = vars a
vars (Exp a)   = vars a
vars (Log a)   = vars a
vars (Sin a)   = vars a
vars (Cos a)   = vars a
vars (Tan a)   = vars a
vars (Sec a)   = vars a
vars (Csc a)   = vars a
vars (Cot a)   = vars a
vars (Asin a)  = vars a
vars (Acos a)  = vars a
vars (Atan a)  = vars a
vars (Asec a)  = vars a
vars (Acsc a)  = vars a
vars (Acot a)  = vars a
vars (Sinh a)  = vars a
vars (Cosh a)  = vars a
vars (Tanh a)  = vars a
vars (Sech a)  = vars a
vars (Csch a)  = vars a
vars (Coth a)  = vars a
vars (Asinh a) = vars a
vars (Acosh a) = vars a
vars (Atanh a) = vars a
vars (Asech a) = vars a
vars (Acsch a) = vars a
vars (Acoth a) = vars a
vars (Sqrt a)  = vars a
vars (Sign a)  = vars a
--Elem for Vars
varIn :: (Eq a, Num a) => Expr a -> Expr a -> Bool
varIn x y = elem x (vars y)
--Evaluate Expressions!
computeExpr :: (Floating a) => Expr a -> a
computeExpr (Const a)  = a
computeExpr (Var _)   = error "Variables In Expr!"
computeExpr (a :+: b) = computeExpr a + computeExpr b
computeExpr (a :-: b) = computeExpr a - computeExpr b
computeExpr (a :*: b) = computeExpr a * computeExpr b
computeExpr (a :/: b) = computeExpr a / computeExpr b
computeExpr (a :^: b) = computeExpr a ** computeExpr b
computeExpr (Parens a) = computeExpr a
computeExpr (Abs a) = abs (computeExpr a)
computeExpr (Exp a) = exp (computeExpr a)
computeExpr (Log a) = log (computeExpr a)
computeExpr (Sin a) = sin (computeExpr a)
computeExpr (Cos a) = cos (computeExpr a)
computeExpr (Tan a) = tan (computeExpr a)
computeExpr (Sec a) = sec (computeExpr a)
computeExpr (Csc a) = csc (computeExpr a)
computeExpr (Cot a) = cot (computeExpr a)
computeExpr (Asin a) = asin (computeExpr a)
computeExpr (Acos a) = acos (computeExpr a)
computeExpr (Atan a) = atan (computeExpr a)
computeExpr (Asec a) = asec (computeExpr a)
computeExpr (Acsc a) = acsc (computeExpr a)
computeExpr (Acot a) = acot (computeExpr a)
computeExpr (Sinh a) = sinh (computeExpr a)
computeExpr (Cosh a) = cosh (computeExpr a)
computeExpr (Tanh a) = tanh (computeExpr a)
computeExpr (Sech a) = sech (computeExpr a)
computeExpr (Csch a) = csch (computeExpr a)
computeExpr (Coth a) = coth (computeExpr a)
computeExpr (Asinh a) = asinh (computeExpr a)
computeExpr (Acosh a) = acosh (computeExpr a)
computeExpr (Atanh a) = atanh (computeExpr a)
computeExpr (Asech a) = asech (computeExpr a)
computeExpr (Acsch a) = acsch (computeExpr a)
computeExpr (Acoth a) = acoth (computeExpr a)
computeExpr (Sqrt a)  = sqrt(computeExpr a)
computeExpr (Sign a)  = signum (computeExpr a)

replaceVar :: (Eq t, Floating t) => Expr t -> Expr t -> t -> Expr t
replaceVar (Const a) _ _ = Const a
replaceVar (a :+: b) var val = replaceVar a var val + replaceVar b var val
replaceVar (a :-: b) var val = replaceVar a var val - replaceVar b var val
replaceVar (a :*: b) var val = replaceVar a var val * replaceVar b var val
replaceVar (a :/: b) var val = replaceVar a var val / replaceVar b var val
replaceVar (a :^: b) var val = replaceVar a var val ** replaceVar b var val
replaceVar (Parens a) var val = replaceVar a var val
replaceVar (Abs a) var val = abs (replaceVar a var val)
replaceVar (Exp a) var val = exp (replaceVar a var val)
replaceVar (Log a) var val = log (replaceVar a var val)
replaceVar (Sin a) var val = sin (replaceVar a var val)
replaceVar (Cos a) var val = cos (replaceVar a var val)
replaceVar (Tan a) var val = tan (replaceVar a var val)
replaceVar (Sec a) var val = sec (replaceVar a var val)
replaceVar (Csc a) var val = csc (replaceVar a var val)
replaceVar (Cot a) var val = cot (replaceVar a var val)
replaceVar (Asin a) var val = asin (replaceVar a var val)
replaceVar (Acos a) var val = acos (replaceVar a var val)
replaceVar (Atan a) var val = atan (replaceVar a var val)
replaceVar (Asec a) var val = asec (replaceVar a var val)
replaceVar (Acsc a) var val = acsc (replaceVar a var val)
replaceVar (Acot a) var val = acot (replaceVar a var val)
replaceVar (Sinh a) var val = sinh (replaceVar a var val)
replaceVar (Cosh a) var val = cosh (replaceVar a var val)
replaceVar (Tanh a) var val = tanh (replaceVar a var val)
replaceVar (Sech a) var val = sech (replaceVar a var val)
replaceVar (Csch a) var val = csch (replaceVar a var val)
replaceVar (Coth a) var val = coth (replaceVar a var val)
replaceVar (Asinh a) var val = asinh (replaceVar a var val)
replaceVar (Acosh a) var val = acosh (replaceVar a var val)
replaceVar (Atanh a) var val = atanh (replaceVar a var val)
replaceVar (Asech a) var val = asech (replaceVar a var val)
replaceVar (Acsch a) var val = acsch (replaceVar a var val)
replaceVar (Acoth a) var val = acoth (replaceVar a var val)
replaceVar (Sqrt a) var val = sqrt(replaceVar a var val)
replaceVar (Sign a) var val = signum (replaceVar a var val)
replaceVar a var val
    | a == var = Const val
    | otherwise = a

replaceList :: (Eq a, Floating a) => [Expr a] -> [a] -> [Expr a -> Expr a]
replaceList = zipWith (flip . flip replaceVar)

evalExpr :: (Eq a, Floating a) => Expr a -> [Expr a] -> [a] -> Expr a
evalExpr = flip (flip . (foldr (.) id .) . replaceList)

eval :: (Eq a, Floating a) => Expr a -> [Expr a] -> [a] -> a
eval = ((computeExpr .) .) . evalExpr
