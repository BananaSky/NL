module Eval where

import Syntax
import Math

printLn a = putStrLn $ show a
printExpression = putStrLn . prettify . simplify

eval :: [Statement] -> [Statement] -> IO ()
eval [] _ = return ()
eval ((DerivateStatement s):ss) statements = do
  putStrLn $ "derivative of " ++ (prettify $ toExpression s statements) ++ " :"
  printExpression $ derivate (toExpression s statements)
  eval ss statements
eval ((IntegrateStatement s):ss) statements = do
  putStrLn $ "integral of " ++ (prettify $ toExpression s statements) ++ " :"
  printExpression $ integrate (toExpression s statements)
  eval ss statements
eval ((SolveStatement e):ss) statements = do
  putStrLn $ "Solving " ++ prettify e
  printExpression $ solve e
  eval ss statements
eval ((Identifier i):ss) statements = do
  eval [find i statements] statements
  eval ss statements
eval ((Assignment i s):ss) statements = do
  putStrLn $ i ++ ": "
  eval [s] statements
  eval ss statements
eval ((Calculation e):ss) statements = do
  print $ prettify e
  eval ss statements
eval ((FunctionCall (Identifier i) args):ss) statements = do
  putStrLn $ i ++ show args ++ ": "
  print $ function statements (toExpression (find i statements) statements) args
  eval ss statements

function :: [Statement] -> Expression -> [Integer] -> Integer
function s (Constant n) args =  n
function s (BinaryExpression Add      e1 e2) args = function s e1 args + function s e2 args
function s (BinaryExpression Multiply e1 e2) args = function s e1 args * function s e2 args
function s (BinaryExpression Divide   e1 e2) args = function s e1 args `div` function s e2 args
function s (BinaryExpression Subtract e1 e2) args = function s e1 args - function s e2 args
function s (BinaryExpression Exponent e1 e2) args = function s e1 args ^ function s e2 args
function statements (Variable v) args = case v of
  "x" -> args !! 0
  "y" -> args !! 1
  "z" -> args !! 2
  s   -> calculate statements $ toExpression (find s statements) statements

prettify :: Expression -> String
prettify (Constant n) = show n
prettify (Variable s) = s
prettify (Fractional f) = show f
prettify (Neg e) = "(-" ++ prettify e ++ ")"
prettify (BinaryExpression Add        e1 e2) = "(" ++ prettify e1 ++ " + " ++ prettify e2 ++ ")"
prettify (BinaryExpression Subtract   e1 e2) = "(" ++ prettify e1 ++ " - " ++ prettify e2 ++ ")"
prettify (BinaryExpression Multiply  (Variable s) (Constant c)) = show c ++ s
prettify (BinaryExpression Multiply  (Constant c) (Variable s)) = show c ++ s
prettify (BinaryExpression Multiply   e1 e2) = "(" ++ prettify e1 ++ " * " ++ prettify e2 ++ ")"
prettify (BinaryExpression Divide     e1 e2) = "(" ++ prettify e1 ++ ") / (" ++ prettify e2 ++ ")"
prettify (BinaryExpression Exponent   e1 e2) = prettify e1 ++ "^(" ++ prettify e2 ++ ")"
prettify (Equality e1 e2) = prettify e1 ++ " = " ++ prettify e2
prettify (Function f e) = show f ++ "(" ++ prettify e ++ ")"

getvars :: Expression -> [Expression]
getvars v@(Variable s)             = [v]
getvars (BinaryExpression _ e1 e2) = getvars e1 ++ getvars e2
getvars _                          = []
