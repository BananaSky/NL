module Eval where

import Syntax

evalCalculation :: Expression -> Int
evalCalculation (Constant n) = fromIntegral n
evalCalculation (BinaryExpression Add      e1 e2) = evalCalculation e1 + evalCalculation e2
evalCalculation (BinaryExpression Multiply e1 e2) = evalCalculation e1 * evalCalculation e2
evalCalculation (BinaryExpression Divide   e1 e2) = evalCalculation e1 `div` evalCalculation e2
evalCalculation (BinaryExpression Subtract e1 e2) = evalCalculation e1 - evalCalculation e2
evalCalculation (BinaryExpression Exponent e1 e2) = evalCalculation e1 ^ evalCalculation e2
evalCalculation _ = undefined

functionCall :: Expression -> [Expression] -> Int
functionCall function args =  undefined

derivate :: Expression -> Expression
derivate (Constant n) = Constant 0
derivate (Variable s) = Constant 1
derivate (BinaryExpression Add      e1 e2) = BinaryExpression Add (derivate e1) (derivate e2)
derivate (BinaryExpression Multiply e1 e2) = BinaryExpression Add udv vdu
  where udv = (BinaryExpression Multiply e1 (derivate e2))
        vdu = (BinaryExpression Multiply (derivate e1) e2)
derivate (BinaryExpression Divide   e1 e2) = BinaryExpression Divide top bottom
  where top    = BinaryExpression Subtract ldh hdl
        hdl    = (BinaryExpression Multiply e1 (derivate e2))
        ldh    = (BinaryExpression Multiply e2 (derivate e1))
        bottom = (BinaryExpression Exponent e2 (Constant 2))
derivate (BinaryExpression Subtract e1 e2) = BinaryExpression Subtract (derivate e1) (derivate e2)
derivate (BinaryExpression Exponent e1 e2@(Constant n)) = BinaryExpression Multiply e2 (BinaryExpression Exponent e1 (Constant (n-1)))
derivate (BinaryExpression Exponent e1 e2) = BinaryExpression Multiply (derivate e2) (BinaryExpression Exponent e1 e2)

simplify :: Expression -> Expression
simplify (Constant n) = (Constant n)
simplify (Variable s) = (Variable s)

--Zero Rules
simplify (BinaryExpression Multiply (Constant 0) _) = (Constant 0)
simplify (BinaryExpression Multiply _ (Constant 0)) = (Constant 0)
simplify (BinaryExpression Exponent _ (Constant 0)) = (Constant 1)
simplify (BinaryExpression Exponent (Constant 0) _) = (Constant 0)
simplify (BinaryExpression Divide   (Constant 0) _) = (Constant 0)
simplify (BinaryExpression Divide   _ (Constant 0)) = undefined
simplify (BinaryExpression Add (Constant 0) e) = e
simplify (BinaryExpression Add e (Constant 0)) = e
simplify (BinaryExpression Subtract (Constant 0) e) = Neg e
simplify (BinaryExpression Subtract e (Constant 0)) = e

--Exponent/Constant Rules
simplify (BinaryExpression Exponent (Constant 1) _) = (Constant 1)
simplify (BinaryExpression Exponent e (Constant 1)) = e
simplify (BinaryExpression Exponent (Constant b) (Constant c)) = (Constant $ b ^ c)

--Multiplication/Associative Rules
simplify (BinaryExpression Multiply (Constant c) (BinaryExpression Divide   x y)) = (BinaryExpression Divide (BinaryExpression Multiply (Constant c) x) y)
simplify (BinaryExpression Multiply (Constant c) (BinaryExpression Exponent x y)) = (BinaryExpression Multiply (Constant c) (BinaryExpression Exponent x y))
simplify (BinaryExpression Multiply (Constant c) (BinaryExpression Multiply x y)) = (BinaryExpression Multiply (BinaryExpression Multiply (Constant c) x) y)
simplify (BinaryExpression Multiply (Constant c) (BinaryExpression anyOp    x y)) = (BinaryExpression anyOp (BinaryExpression Multiply (Constant c) x) (BinaryExpression Multiply (Constant c) y))

simplify (BinaryExpression Multiply (BinaryExpression Divide   x y) (Constant c)) = (BinaryExpression Divide (BinaryExpression Multiply (Constant c) x) y)
simplify (BinaryExpression Multiply (BinaryExpression Exponent x y) (Constant c)) = (BinaryExpression Multiply (BinaryExpression Exponent x y) (Constant c))
simplify (BinaryExpression Multiply (BinaryExpression Multiply x y) (Constant c)) = (BinaryExpression Multiply (BinaryExpression Multiply (Constant c) x) y)
simplify (BinaryExpression Multiply (BinaryExpression anyOp    x y) (Constant c)) = (BinaryExpression anyOp (BinaryExpression Multiply (Constant c) x) (BinaryExpression Multiply (Constant c) y))

--Rules for constants
simplify (BinaryExpression Add (Constant b) (Constant c)) = (Constant $ b + c)
simplify (BinaryExpression Subtract (Constant b) (Constant c)) = (Constant $ b - c)
simplify (BinaryExpression Multiply (Constant b) (Constant c)) = (Constant $ b * c)
simplify (BinaryExpression Divide   (Constant b) (Constant c)) = (Constant $ b `div` c)

--Generic rule
simplify (BinaryExpression binop e1 e2) = (BinaryExpression binop (simplify e1) (simplify e2))

prettify :: Expression -> String
prettify (Constant n) = show n
prettify (Variable s) = s
prettify (Neg e) = "(-" ++ prettify e ++ ")"
prettify (BinaryExpression Add        e1 e2) = prettify e1 ++ " + " ++ prettify e2
prettify (BinaryExpression Multiply  (Variable s) (Constant c)) = show c ++ s
prettify (BinaryExpression Multiply  (Constant c) (Variable s)) = show c ++ s
prettify (BinaryExpression Multiply   e1 e2) = prettify e1 ++ " * " ++ prettify e2
prettify (BinaryExpression Divide     e1 e2) = "(" ++ prettify e1 ++ ") / (" ++ prettify e2 ++ ")"
prettify (BinaryExpression Exponent   e1 e2) = prettify e1 ++ "^" ++ prettify e2

getvars :: Expression -> [Expression]
getvars v@(Variable s)             = [v]
getvars (BinaryExpression _ e1 e2) = getvars e1 ++ getvars e2
getvars _                          = []
