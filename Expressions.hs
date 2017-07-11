module Expressions where
import MoreTrig

infixr 5 :+:
infixr 5 :-:
infixr 7 :*:
infixr 7 :/:
infixl 8 :^:

data Expr a = Var [Char]
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
    Const a == Var b = False
    Var a == Const b = False
    Parens (a) == Parens (b) = a == b
    Parens (a) == b = a == b
    a == Parens (b) = a == b 
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
    (a :-: b) == (c :-: d) = ((a == c) && (b == d))
    (a :/: b) == (c :/: d) = ((a == c) && (b == d))
    (a :^: b) == (c :^: d) = ((a == c) && (b == d))
    _ == _ = False --Give Up!

instance (Show m) => Show (Expr m) where
    show ( Const a ) = show (a)
    show ( Var a )   = id (a)
    show (a :+: b)   = show (a) ++ " + " ++ show (b) 
    show (a :*: b)   = show (a) ++ " * " ++ show (b) 
    show (a :-: b)   = show (a) ++ " - " ++ show (b) 
    show (a :/: b)   = show (a) ++ " / " ++ show (b) 
    show (a :^: b)   = show (a) ++ " ^ " ++ show (b)
    show ( Parens a) = "( " ++ show (a) ++ " )" 
    show ( Abs a)    = "| " ++ show (a) ++ " |"
    show ( Log a)    = "Log ( " ++ show (a) ++ " )" 
    show ( Exp a)    = "e ^ ( " ++ show (a) ++ " )" 
    show ( Sin a)    = "Sin ( " ++ show (a) ++ " )" 
    show ( Cos a)    = "Cos ( " ++ show (a) ++ " )" 
    show ( Tan a)    = "Tan ( " ++ show (a) ++ " )" 
    show ( Sec a)    = "Sec ( " ++ show (a) ++ " )" 
    show ( Csc a)    = "Csc ( " ++ show (a) ++ " )" 
    show ( Cot a)    = "Cot ( " ++ show (a) ++ " )" 
    show ( Asin a)   = "ASin ( " ++ show (a) ++ " )" 
    show ( Acos a)   = "ACos ( " ++ show (a) ++ " )" 
    show ( Atan a)   = "ATan ( " ++ show (a) ++ " )"
    show ( Asec a)    = "ASec ( " ++ show (a) ++ " )" 
    show ( Acsc a)    = "ACsc ( " ++ show (a) ++ " )" 
    show ( Acot a)    = "ACot ( " ++ show (a) ++ " )"  
    show ( Sinh a)   = "Sinh ( " ++ show (a) ++ " )"
    show ( Cosh a)   = "Cosh ( " ++ show (a) ++ " )"
    show ( Tanh a)   = "Tanh ( " ++ show (a) ++ " )"
    show ( Sech a)    = "Sech ( " ++ show (a) ++ " )" 
    show ( Csch a)    = "Csch ( " ++ show (a) ++ " )" 
    show ( Coth a)    = "Coth ( " ++ show (a) ++ " )"  
    show ( Asinh a)  = "ASinh ( " ++ show (a) ++ " )"
    show ( Acosh a)  = "Acosh ( " ++ show (a) ++ " )" 
    show ( Atanh a)  = "Atanh ( " ++ show (a) ++ " )"
    show ( Asech a)    = "ASech ( " ++ show (a) ++ " )" 
    show ( Acsch a)    = "ACsch ( " ++ show (a) ++ " )" 
    show ( Acoth a)    = "ACoth ( " ++ show (a) ++ " )" 
    show ( Sqrt a)   = "√( " ++ show (a) ++ " )"
    show ( Sign a)   = "Sign (" ++ show (a) ++ " )"

instance (Num m, Eq m) => Num (Expr m) where
    a + b = (a :+: b)
    a * Const b = Const b :*: a --Makes Things Easier if Constants go first
    a * b
        | (((getPrec b) >= 7) && ((getPrec a) >= 7)) = a :*: b
        | ((getPrec b) >= 7) = Parens(a) :*: (b)
        | ((getPrec a) >= 7) = (a) :*: Parens (b)
        | otherwise = Parens (a) :*: Parens (b)
    a - b = (a :-: b)
    negate (Const a) = Const ( -1 * a)
    negate (a) = Const (-1) * a
    abs (Const a) = Const (abs a)
    abs (a :*: Const (-1)) = a
    abs a = Abs a
    signum (Const a) = Const (signum a)
    signum a = Sign a
    fromInteger a = Const (fromInteger a)
instance (Fractional m, Eq m) => Fractional (Expr m) where
    a / b
        | (((getPrec b) >= 7) && ((getPrec a) >= 7)) = a :/: b
        | ((getPrec b) >= 7) = Parens(a) :/: (b)
        | ((getPrec a) >= 7) = (a) :/: Parens (b)
        | otherwise = Parens (a) :/: Parens (b)
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
        | (((getPrec b) >= 8) && ((getPrec a) >= 8)) = a :^: b
        | ((getPrec b) >= 8) = Parens(a) :^: (b)
        | ((getPrec a) >= 8) = (a) :^: Parens (b)
        | otherwise = Parens (a) :^: Parens (b)

-- Get the precedence of the outermost expression of a given operation
getPrec :: (Num a) => Expr a1 -> a
getPrec (_ :+: _) = 5
getPrec (_ :-: _) = 5
getPrec (_ :*: _) = 7
getPrec (_ :/: _) = 7
getPrec (_ :^: _) = 8
getPrec _ = 10

--Evaluate Expressions!
evalExpr :: (Floating a) => Expr a -> a -> a
evalExpr (Const a) _ = a
evalExpr (Var _)   c = c
evalExpr (a :+: b) c = evalExpr a c + evalExpr b c 
evalExpr (a :-: b) c = evalExpr a c - evalExpr b c 
evalExpr (a :*: b) c = evalExpr a c * evalExpr b c 
evalExpr (a :/: b) c = evalExpr a c / evalExpr b c
evalExpr (a :^: b) c = evalExpr a c ** evalExpr b c
evalExpr (Parens a) c = evalExpr a c
evalExpr (Abs a)   c = abs (evalExpr a c)
evalExpr (Exp a)   c = exp (evalExpr a c)
evalExpr (Log a)   c = log (evalExpr a c)
evalExpr (Sin a)   c = sin (evalExpr a c)
evalExpr (Cos a)   c = cos (evalExpr a c)
evalExpr (Tan a)   c = tan (evalExpr a c)
evalExpr (Sec a)   c = sec (evalExpr a c)
evalExpr (Csc a)   c = csc (evalExpr a c)
evalExpr (Cot a)   c = cot (evalExpr a c)
evalExpr (Asin a)   c = asin (evalExpr a c)
evalExpr (Acos a)   c = acos (evalExpr a c)
evalExpr (Atan a)   c = atan (evalExpr a c)
evalExpr (Asec a)   c = asec (evalExpr a c)
evalExpr (Acsc a)   c = acsc (evalExpr a c)
evalExpr (Acot a)   c = acot (evalExpr a c)
evalExpr (Sinh a)   c = sinh (evalExpr a c)
evalExpr (Cosh a)   c = cosh (evalExpr a c)
evalExpr (Tanh a)   c = tanh (evalExpr a c)
evalExpr (Sech a)   c = sech (evalExpr a c)
evalExpr (Csch a)   c = csch (evalExpr a c)
evalExpr (Coth a)   c = coth (evalExpr a c)
evalExpr (Asinh a)   c = asinh (evalExpr a c)
evalExpr (Acosh a)   c = acosh (evalExpr a c)
evalExpr (Atanh a)   c = atanh (evalExpr a c)
evalExpr (Asech a)   c = asech (evalExpr a c)
evalExpr (Acsch a)   c = acsch (evalExpr a c)
evalExpr (Acoth a)   c = acoth (evalExpr a c)
evalExpr (Sqrt a)    c = sqrt(evalExpr a c)
evalExpr (Sign a)    c = signum (evalExpr a c)
