
--{Options }-
module Simplification where

import Expressions
import MoreTrig
simplify :: (Floating a, Eq a) => Expr a -> Expr a
--Constant Simplication!
simplify (Const a :+: Const b) = Const $ a + b
simplify (Const a :*: Const b) = Const $ a * b
simplify (Const a :-: Const b) = Const $ a - b
simplify (Const a :/: Const b) = Const $ a / b
simplify (Const a :^: Const b) = Const $ a ** b
simplify (Sqrt (Const a)) = Const (sqrt a)
simplify (Exp (Const a))  = Const (exp a)
simplify (Log (Const a))  = Const (log a)
simplify (Sin (Const a))  = Const (sin a)
simplify (Cos (Const a))  = Const (cos a)
simplify (Tan (Const a))  = Const (tan a)
simplify (Sec (Const a))  = Const (sec a)
simplify (Csc (Const a))  = Const (csc a)
simplify (Cot (Const a))  = Const (cot a)
simplify (Asin (Const a)) = Const (asin a)
simplify (Acos (Const a)) = Const (acos a)
simplify (Atan (Const a)) = Const (atan a)
simplify (Asec (Const a))  = Const (asec a)
simplify (Acsc (Const a))  = Const (acsc a)
simplify (Acot (Const a))  = Const (acot a)
simplify (Sinh (Const a)) = Const (sinh a)
simplify (Cosh (Const a)) = Const (cosh a)
simplify (Tanh (Const a)) = Const (tanh a)
simplify (Sech (Const a))  = Const (sech a)
simplify (Csch (Const a))  = Const (csch a)
simplify (Coth (Const a))  = Const (coth a)
simplify (Asinh (Const a)) = Const (asinh a)
simplify (Acosh (Const a)) = Const (acosh a)
simplify (Atanh (Const a)) = Const (atanh a)
simplify (Asech (Const a))  = Const (asech a)
simplify (Acsch (Const a))  = Const (acsch a)
simplify (Acoth (Const a))  = Const (acoth a)
-- Distributive Rule
simplify (a :*: Parens (b :+: c)) = (a :*: b) :+: (a :*: c)
simplify (a :*: Parens (b :-: c)) = ( a :*: b) :-: ( a :*: c)
simplify (Parens (b :+: c) :*: a) = (a :*: b) :+: (a :*: c)
simplify (Parens (b :-: c) :*: a) = ( a :*: b) :-: ( a :*: c)
-- Values That Can be simplified away!
simplify (a :+: Const 0) = simplify a
simplify (Const 0 :+: a) = simplify a
simplify (a :-: Const 0) = simplify a
simplify (Const 0 :-: a) = simplify (negate a)
simplify (Const 1 :*: a) = simplify a
simplify (a :*: Const 1) = simplify a
simplify (_ :*: Const 0) = Const 0
simplify (Const 0 :*: _) = Const 0
simplify (Const 0 :/: _) = Const 0
simplify (a :^: Const 1)               = simplify a
simplify (_ :^: Const 0)               = Const 1
simplify (a :/: Const 1) = simplify a
simplify (_ :/: Const 0) = error "Jeeze Man, Watch Out! You're Dividing By A Zero!"
simplify ((a :*: b) :/: (c :*: d)) | a == c = simplify (b :/: d)
                                   | a == d = simplify (b :/: c)
                                   | b == c = simplify (a :/: d)
                                   | b == d = simplify (a :/: c)
simplify ((a :/: b) :*: (c :/: d)) | a == c = simplify (b :*: d)
                                   | a == d = simplify (b :*: c)
                                   | b == c = simplify (a :*: d)
                                   | b == d = simplify (a :*: c)

simplify (a :-: b) |
  a == b = Const 0


simplify ( a :/: b)   | a == b = Const 1 -- only when a == b
simplify ( a :*: b)   | a == b = a :^: Const 2 -- only when a == b
simplify (a :*: b :^: c) | a == b = b :^: (c :+: Const 1)
simplify ( a :*: (b :/: c))  | b == c = a -- only when a == b
                             | a == c = b
                             | otherwise = simplify (a :*: b) :/: simplify c
-- simplify ( (a :*: b) :/: c)  | b == c = a -- only when a == b
--                              | a == c = b
--                              | otherwise = simplify (a :*: b) :/: simplify (b)
-- simplify ( a :/: b :/: c)    | a == b = c
--                              | b == c = a

-- Some Rules!
simplify (Const a :*: (Const b :*: expr)) = (Const $ a * b) :*: simplify expr
simplify (Const a :*: expr :*: Const b)   = (Const $ a * b) :*: simplify expr
simplify (expr :*: Const a :*: Const b)   = (Const $ a * b) :*: simplify expr
---- Learing Exp and Logs
simplify ( Exp (a :+: b))       = Exp ( simplify a) :*: Exp (simplify b)
simplify (Log  (a :^: Const b)) = Const b :*: Log (simplify a)
simplify (Log  (a :*: b))       = Log ( simplify a) :+: Log( simplify b)
simplify (Log  (a :/: b))       = Log ( simplify a) :-: Log( simplify b)
simplify (Exp (Log a))          = simplify a
simplify (Exp (Log a :*: b))    = simplify a :^: simplify b
-- Square Roots
simplify (Sqrt (a :^: Const 2)) = Abs a
simplify (Sqrt a :^: Const 2) = a
simplify (Abs a :/: b) | a == b = Sign a
simplify (a :/: Sign b) | a == b = Sign a
--Trig Stuff
simplify (a :/: Sin b) = simplify a :*: Csc (simplify b)
simplify (a :/: Cos b) = simplify a :*: Sec (simplify b)
simplify (a :/: Tan b) = simplify a :*: Cot (simplify b)
simplify (a :/: Sinh b) = simplify a :*: Csch (simplify b)
simplify (a :/: Cosh b) = simplify a :*: Sech (simplify b)
simplify (a :/: Tanh b) = simplify a :*: Coth (simplify b)
simplify (a :/: Csc b) = simplify a :*: Sin (simplify b)
simplify (a :/: Sec b) = simplify a :*: Cos (simplify b)
simplify (a :/: Cot b) = simplify a :*: Tan (simplify b)
simplify (a :/: Csch b) = simplify a :*: Sinh (simplify b)
simplify (a :/: Sech b) = simplify a :*: Cosh (simplify b)
simplify (a :/: Coth b) = simplify a :*: Tanh (simplify b)
simplify ( Asin (Const a :/: b)) = Acsc (Const a :*: simplify b)
simplify ( Acos (Const a :/: b)) = Asec (Const a :*: simplify b)
simplify ( Atan (Const a :/: b)) = Acot (Const a :*: simplify b)
simplify ( Asec (Const a :/: b)) = Acos (Const a :*: simplify b)
simplify ( Acsc (Const a :/: b)) = Asin (Const a :*: simplify b)
simplify ( Acot (Const a :/: b)) = Atan (Const a :*: simplify b)
simplify ( Asinh (Const a :/: b)) = Acsch (Const a :*: simplify b)
simplify ( Acosh (Const a :/: b)) = Asech (Const a :*: simplify b)
simplify ( Atanh (Const a :/: b)) = Acoth (Const a :*: simplify b)
simplify ( Asech (Const a :/: b)) = Acosh (Const a :*: simplify b)
simplify ( Acsch (Const a :/: b)) = Asinh (Const a :*: simplify b)
simplify ( Acoth (Const a :/: b)) = Atanh (Const a :*: simplify b)
simplify ((Sin a :^: Const 2) :+: (Cos b :^: Const 2)) | a == b = Const 1
simplify (Const 1 :-: (Cos a :^: Const 2)) = Sin a :^: Const 2
simplify (Const 1 :-: (Sin a :^: Const 2)) = Cos a :^: Const 2
-- Basics!
simplify (Parens a :+: Parens b)
        | getPrec b >= 4 && (getPrec a >= 4) = simplify a :+: simplify b
        | getPrec b >= 4 = Parens (simplify a) :+: simplify b
        | getPrec a >= 4 = simplify a :+: Parens (simplify b)
        | otherwise = Parens ( simplify a) :+: Parens (simplify b)
simplify (Parens a :-: Parens b)
        | getPrec b >= 5 && getPrec a >= 5 = simplify a :-: simplify b
        | getPrec b >= 5 = Parens (simplify a) :-: simplify b
        | getPrec a >= 5 = simplify a :-: Parens (simplify b)
        | otherwise = Parens ( simplify a) :-: Parens (simplify b)
simplify (Parens a :*: Parens b)
        | getPrec b >= 6 && getPrec a >= 6 = simplify a :*: simplify b
        | getPrec b >= 6 = Parens (simplify a) :*: simplify b
        | getPrec a >= 6 = simplify a :*: Parens (simplify b)
        | otherwise = Parens ( simplify a) :*: Parens (simplify b)
simplify (Parens a :/: Parens b)
        | getPrec b >= 7 && getPrec a >= 7 = simplify a :/: simplify b
        | getPrec b >= 7 = Parens (simplify a) :/: simplify b
        | getPrec a >= 7 = simplify a :/: Parens (simplify b)
        | otherwise = Parens ( simplify a) :/: Parens (simplify b)
simplify (Parens a :^: Parens b)
        | getPrec b >= 8 && getPrec a >= 8 = simplify a :^: simplify b
        | getPrec b >= 8 = Parens (simplify a) :^: simplify b
        | getPrec a >= 8 = simplify a :^: Parens (simplify b)
        | otherwise = Parens ( simplify a) :^: Parens (simplify b)
simplify (a :+: b) = simplify a :+: simplify b
simplify (a :-: b) = simplify a :-: simplify b
simplify (a :*: b) = simplify a :*: simplify b
simplify (a :/: b) = simplify a :/: simplify b
simplify (a :^: b) = simplify a :^: simplify b
-- Peeling Functions Away
simplify (Parens a) = Parens (simplify a)
simplify (Sqrt a) = Sqrt (simplify a)
simplify (Exp a)  = Exp (simplify a)
simplify (Log a)  = Log (simplify a)
simplify (Sin a)  = Sin (simplify a)
simplify (Cos a)  = Cos (simplify a)
simplify (Tan a)  = Tan (simplify a)
simplify (Sec a)  = Sec (simplify a)
simplify (Csc a)  = Csc (simplify a)
simplify (Cot a)  = Cot (simplify a)
simplify (Asin a)  = Asin (simplify a)
simplify (Acos a)  = Acos (simplify a)
simplify (Atan a)  = Atan (simplify a)
simplify (Asec a)  = Asec (simplify a)
simplify (Acsc a)  = Acsc (simplify a)
simplify (Acot a)  = Acot (simplify a)
simplify (Sech a)  = Sech (simplify a)
simplify (Csch a)  = Csch (simplify a)
simplify (Coth a)  = Coth (simplify a)
simplify (Asinh a)  = Asinh (simplify a)
simplify (Acosh a)  = Acosh (simplify a)
simplify (Atanh a)  = Atanh (simplify a)
simplify (Asech a)  = Asech (simplify a)
simplify (Acsch a)  = Acsch (simplify a)
simplify (Acoth a)  = Acoth (simplify a)
simplify a = a
-- Repeat simplification untill it stops changing
fullSimplify :: (Floating a, Eq a) => Expr a -> Expr a
fullSimplify expr = fullSimplify' expr (Const 0) -- placeholder
  where fullSimplify' cur lastone | cur == lastone = cur
                               | otherwise = let cur' = simplify cur
                                             in fullSimplify' cur' cur
simp :: (Floating a, Eq a) => Expr a -> Expr a
simp = fullSimplify