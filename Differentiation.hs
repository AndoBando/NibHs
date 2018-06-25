
module Differentiation where
import Simplification
import Expressions
-- Define Differentiation
derivative :: (Floating a, Eq a) => Expr a -> Expr a -> Expr a
derivative (Var v) (Var a)
    | v == a = Const 1
    | otherwise = Const 0
derivative _ (Const _)         = Const 0
derivative v (Const a :*: b)   = Const a :*: derivative v b
derivative v (a :/: Const b)   = derivative v a :/: Const b

-- Sum Rule
derivative v (a :+: b) = derivative v a + derivative v b
-- Differnce Rule
derivative v (a :-: b) = derivative v a - derivative v b
-- Product rule (ab' + a'b)
derivative v (a :*: b)
    | v `varIn` a && v `varIn` b = a * derivative v b :+:  b * derivative v a
    | v `varIn` a = derivative v a * b
    | v `varIn` b = a * derivative v b
    | otherwise = a * b
-- Quotient rule
derivative v (a :/: b)
    | v `varIn` a && v `varIn` b = (derivative v a * b - derivative v b * a) / b ** Const 2
    | v `varIn` a = derivative v a / b
    | v `varIn` b = Const (-1) * (a * derivative v b) / b ** Const 2
    | otherwise = a / b
-- Power Rule
derivative v (a :^: Const x)
    | v `varIn` a = Const x * a ** Const (x-1) * derivative v a
    | otherwise   = Const 0
-- Natural Logs
derivative v (Log a)
    | v `varIn` a = (Const 1 / a) * derivative v a
    | otherwise = Const 0
derivative v (Exp a)
    | v `varIn` a = Exp a * derivative v a
    | otherwise = Const 0
derivative v (a :^: b )
    | v `varIn` a && v `varIn` b = derivative v (Exp ( Log a * b))
    | v `varIn` a = b * derivative v a * a ** (b - Const 1)
    | v `varIn` b = Log a * derivative v b * a ** b
--Square Roots
derivative v (Sqrt a)
    | v `varIn` a = derivative v a / (Const 2 * Sqrt a)
    | otherwise = Const 0
derivative v (Abs a)
    | v `varIn` a = Sign a * derivative v a
    | otherwise = Const 0
derivative v (Sign _)  = Const 0
-- Trig
derivative v (Sin a)
    | v `varIn` a  = Cos a * derivative v a
    | otherwise = Const 0
derivative v (Cos a)
    | v `varIn` a  = negate $ Sin a * derivative v a
    | otherwise = Const 0
derivative v (Tan a)
    | v `varIn` a  = Sec a ** Const 2 * derivative v a
    | otherwise = Const 0
derivative v (Sec a)
    | v `varIn` a  = Sec a * Tan a * derivative v a
    | otherwise = Const 0
derivative v (Csc a)
    | v `varIn` a  = negate $ Csc a * Cot a * derivative v a
    | otherwise = Const 0
derivative v (Cot a)
    | v `varIn` a  = negate $ Csc a ** 2 * derivative v a
    | otherwise = Const 0
-- HyperBolic Trig
derivative v (Sinh a)
    | v `varIn` a  = Cosh a * derivative v a
    | otherwise = Const 0
derivative v (Cosh a)
    | v `varIn` a  = Sinh a * derivative v a
    | otherwise = Const 0
derivative v (Tanh a)
    | v `varIn` a  = Sech a ** Const 2 * derivative v a
    | otherwise = Const 0
derivative v (Sech a)
    | v `varIn` a  = negate $ Sech a * Tanh a * derivative v a
    | otherwise = Const 0
derivative v (Csch a)
    | v `varIn` a  = negate $  Csch a * Coth a * derivative v a
    | otherwise = Const 0
derivative v (Coth a)
    | v `varIn` a  = negate $   Csch a ** 2 * derivative v a
    | otherwise = Const 0
-- Inverse Trig
derivative v (Asin a)
    | v `varIn` a  = Const 1 / Sqrt (Const 1 - a ** Const 2) * derivative v a
    | otherwise = Const 0
derivative v (Acos a)
    | v `varIn` a  = Const (-1) / Sqrt (Const 1 - a ** Const 2) * derivative v a
    | otherwise = Const 0
derivative v (Atan a)
    | v `varIn` a  = Const 1 / (Const 1 + a ** Const 2) * derivative v a
    | otherwise = Const 0
derivative v (Asec a)
    | v `varIn` a  = Const 1 / (Abs a * Sqrt( a ** Const 2 - Const 1)) * derivative v a
    | otherwise = Const 0
derivative v (Acsc a)
    | v `varIn` a  = Const (-1) / (Abs a * Sqrt( a ** Const 2 - Const 1)) * derivative v a
    | otherwise = Const 0
derivative v (Acot a)
    | v `varIn` a  = Const (-1) / (Const 1 + a ** Const 2) * derivative v a
    | otherwise = Const 0
-- HyperBolic Trig
derivative v (Asinh a)
    | v `varIn` a  = Const 1 / Sqrt (a ** Const 2 :+: Const 1) * derivative v a
    | otherwise = Const 0
derivative v (Acosh a)
    | v `varIn` a  = Const 1 / Sqrt (a ** Const 2 - Const 1) * derivative v a
    | otherwise = Const 0
derivative v (Atanh a)
    | v `varIn` a  = Const 1 / (Const 1 - a ** Const 2) * derivative v a
    | otherwise = Const 0
derivative v (Asech a)
    | v `varIn` a  = Const (-1) / ( a * Sqrt( Const 1 - a ** Const 2)) * derivative v a
    | otherwise = Const 0
derivative v (Acsch a)
    | v `varIn` a  = Const (-1) / ( Abs a * Sqrt( Const 1 - a ** Const 2)) * derivative v a
    | otherwise = Const 0
derivative v (Acoth a)
    | v `varIn` a  = Const 1 / (Const 1 - a ** Const 2) * derivative v a
    | otherwise = Const 0
--Other Operations
derivative _ _              = error "unsupported operation"

deriv :: (Eq a, Floating a) => Expr a -> Expr a -> Expr a
deriv = (simp .) . (. simp) . derivative

diff :: (Eq a, Floating a) => Expr a -> [Expr a]
diff expr = map (($ expr) . deriv) (vars expr)

difflab :: (Eq a, Floating a) => Expr a -> [(Expr a, Expr a)]
difflab expr = zip (vars expr) (map (($ expr) . deriv) (vars expr))

grad :: (Eq a, Floating a) => Expr a -> [a] -> [a]
grad expr loc = map (($ loc).($ vars expr) . eval) $ diff expr

ddx' :: (Eq a, Floating a) => (Expr a -> Expr a) -> a -> Expr a
ddx' f = replaceVar (deriv (Var "x") (f $ Var "x")) (Var "x")


ddx :: (Eq a, Floating a) => (Expr a -> Expr a) -> a -> a
ddx f x = computeExpr ( ddx' f x)

derivs :: (Floating a, Eq a) => Expr a -> Expr a -> [Expr a]
derivs var = iterate (deriv var)

ddxs' :: (Floating a, Eq a) => (Expr a -> Expr a) -> a -> [Expr a]
ddxs' f x = map (($ x) . (`replaceVar` Var "x")) (derivs  (Var "x") (f $ Var "x"))

ddxs :: (Floating a, Eq a) => (Expr a -> Expr a) -> a -> [a]
ddxs f x = map computeExpr (ddxs' f x)

--Taylor Series!!!!!
taylorCoeff :: (Floating b, Eq b) => (Expr b -> Expr b) -> b -> [b]
taylorCoeff = ddxs

macLaurinCoeff :: (Floating b, Eq b) => (Expr b -> Expr b) -> [b]
macLaurinCoeff f = ddxs f 0

-- Variables
α = Var "α"
β = Var "β"
γ = Var "γ"
δ = Var "δ"
ε = Var "ε"
ζ = Var "ζ"
η = Var "η"

--Maximise
increase expr step = zipWith (+) <*> (map (step *) . grad expr)
maximize expr step iter = (!! iter) . iterate (increase expr step)