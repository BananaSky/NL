module Eval where

import Syntax

test = BinaryExpression Add (BinaryExpression Exponent (Variable "x") (Constant 2)) (Constant 2)

evalCalculation :: Expression -> Int
evalCalculation (Constant n) = fromIntegral n
evalCalculation (BinaryExpression Add      e1 e2) = evalCalculation e1 + evalCalculation e2
evalCalculation (BinaryExpression Multiply e1 e2) = evalCalculation e1 * evalCalculation e2
evalCalculation (BinaryExpression Divide   e1 e2) = evalCalculation e1 `div` evalCalculation e2
evalCalculation (BinaryExpression Subtract e1 e2) = evalCalculation e1 - evalCalculation e2
evalCalculation (BinaryExpression Exponent e1 e2) = evalCalculation e1 ^ evalCalculation e2
evalCalculation _ = undefined

doTest = print $ derivate test

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
