# NibHs
Haskell Symbolic Math

## Expressions

The basic data type in NibHs is the Expression or Expr

Expressions are made up of a few basic componets:

### Constants

Constants are 

All variables in Expressions must have the keyword `Var` before them and their names must be *strings* (wrapped in quotation marks)
for example

    expr = Sin $ Var "x"
