module MoreTrig where

sec :: Floating a => a -> a
sec x = 1 / cos x

csc :: Floating a => a -> a
csc x = 1 / sin x

cot :: Floating a => a -> a
cot x = 1 / tan x

asec :: Floating a => a -> a
asec x = acos (1 / x)

acsc :: Floating a => a -> a
acsc x = asin (1 / x)

acot :: Floating a => a -> a
acot x = atan (1 / x)

sech :: Floating a => a -> a
sech x = 1 / cosh x

csch :: Floating a => a -> a
csch x = 1 / sinh x

coth :: Floating a => a -> a
coth x = 1 / tanh x

asech :: Floating a => a -> a
asech x = acosh (1 / x)

acsch :: Floating a => a -> a
acsch x = asinh (1 / x)

acoth :: Floating a => a -> a
acoth x = atanh (1 / x)