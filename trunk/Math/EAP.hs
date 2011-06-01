module Math.EAP where

import Control.Monad.Writer

data Expression = Constant Rational
                | Variable String
                | Expression :+: Expression
                | Expression :-: Expression
                | Expression :*: Expression
                | Expression :/: Expression
                | Negate Expression
                | Abs    Expression
                | Signum Expression
                | Solve String Equation
                  deriving (Show, Eq)

data Equation = Expression :=: Expression deriving (Show, Eq)

instance Num Expression where
    (+)    = (:+:)
    negate = Negate
    (*)    = (:*:)
    fromInteger = Constant . fromInteger
    abs    = Abs
    signum = Signum

type Section = (Integer, Integer)

evaluateExpression :: Expression -> (Expression, [Section])
evaluateExpression x = case runWriter (eval x) of (y, ss) -> (y, coalesce ss)

eval :: Expression -> Writer [Section] Expression
eval (Negate x) = do
  x' <- eval x
  
eval x = return x

coalesce :: [Section] -> [Section]
coalesce = id
