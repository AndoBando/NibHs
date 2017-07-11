
module Differentiation (
    ddx,ddxs,deriv,derivs,taylorCoeff,macLaurinCoeff) where
import Simplification
import Expressions
-- Define Differentiation
derivative :: (Num a, Eq a) => Expr a -> Expr a
derivative (Var _)           = Const 1
derivative (Const _)         = Const 0
derivative (Const a :*: b)   = Const a :*: derivative b
derivative (a :/: Const b)   = derivative a :/: Const b

-- Sum Rule
derivative (a :+: b) = derivative a :+: derivative b
-- Differnce Rule
derivative (a :-: b) = derivative a :-: derivative b
-- Product rule (ab' + a'b)
derivative (a :*: b) = a :*: derivative b :+:  b :*: derivative a
-- Quotient rule
derivative (a :/: b) = (derivative a :*: b :-: derivative b :*: a)
                       :/:
                       b :^: Const 2
-- Power Rule
derivative (a :^: Const x) = Const x :*: a :^: Const (x-1) :*: derivative a
-- Natural Logs
derivative (Log a)    = (Const 1 :/: a) :*: derivative a
derivative (Exp a)    = Exp a :*: derivative a
derivative (a :^: b ) = derivative (Exp ( Log a :*: b))
--Square Roots
derivative (Sqrt a)  = derivative a :/: (Const 2 :*: Sqrt a)
derivative (Abs a)   = Sign a :*: derivative a
derivative (Sign _)  = Const 0
-- Trig
derivative (Sin a)   = Cos a :*: derivative a
derivative (Cos a)   = negate $ Sin a :*: derivative a
derivative (Tan a)   = Sec a :^: Const 2 :*: derivative a
derivative (Sec a)   = Sec a :*: Tan a :*: derivative a
derivative (Csc a)   = negate $ Csc a :*: Cot a :*: derivative a
derivative (Cot a)   = negate $ Csc a :^: 2 :*: derivative a
-- HyperBolic Trig
derivative (Sinh a)   = Cosh a :*: derivative a
derivative (Cosh a)   = Sinh a :*: derivative a
derivative (Tanh a)   = Sech a :^: Const 2 :*: derivative a
derivative (Sech a)   = negate $  Sech a :*: Tanh a :*: derivative a
derivative (Csch a)   = negate $  Csch a :*: Coth a :*: derivative a
derivative (Coth a)   = negate $   Csch a :^: 2 :*: derivative a
-- Inverse Trig
derivative (Asin a)   = Const 1 :/: Sqrt (Const 1 :-: a :^: Const 2) :*: derivative a
derivative (Acos a)   = negate $  Sqrt (Const 1 :-: a :^: Const 2) :*: derivative a
derivative (Atan a)   = Const 1 :/: (Const 1 :+: a :^: Const 2) :*: derivative a
derivative (Asec a)   = Const 1 :/: (Abs a :*: Sqrt( a :^: Const 2 :-: Const 1)) :*: derivative a
derivative (Acsc a)   = negate $  (Abs a :*: Sqrt( a :^: Const 2 :-: Const 1)) :*: derivative a
derivative (Acot a)   = negate $  (Const 1 :+: a :^: Const 2) :*: derivative a
-- HyperBolic Trig
derivative (Asinh a)   =  Const 1 :/: Sqrt (a :^: Const 2 :+: Const 1) :*: derivative a
derivative (Acosh a)   = Const 1 :/: Sqrt (a :^: Const 2 :-: Const 1) :*: derivative a
derivative (Atanh a)   = Const 1 :/: (Const 1 :-: a :^: Const 2) :*: derivative a
derivative (Asech a)   = negate $  ( a :*: Sqrt( Const 1 :-: a :^: Const 2)) :*: derivative a
derivative (Acsch a)   = negate $  ( Abs a :*: Sqrt( Const 1 :-: a :^: Const 2)) :*: derivative a
derivative (Acoth a)   = Const 1 :/: (Const 1 :-: a :^: Const 2) :*: derivative a
--Other Operations
derivative _              = error "unsupported operation"

deriv :: (Eq a, Floating a) => Expr a -> Expr a
deriv = simp . derivative . simp

ddx :: (Eq a, Floating a) => (Expr a -> Expr a) -> a -> a
ddx f = evalExpr . deriv . f $ Var "x"

derivs :: (Floating a, Eq a) => Expr a -> [Expr a]
derivs = iterate deriv

ddxs :: (Floating a, Eq a) => (Expr a1 -> Expr a) -> [a -> a]
ddxs f = fmap evalExpr ((derivs . f) $ Var "x")

--Taylor Series!!!!!
taylorCoeff :: (Floating b, Eq b) => (Expr b -> Expr b) -> b -> [b]
taylorCoeff f a = ddxs f <*> [a]

macLaurinCoeff :: (Floating b, Eq b) => (Expr b -> Expr b) -> [b]
macLaurinCoeff f = ddxs f <*> [0]
