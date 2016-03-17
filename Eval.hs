module Eval where

import Syntax

infixl 7 |*|
infixl 7 |/|
infixl 8 |^|
infixl 6 |+|
infixl 6 |-|

(|*|) e1 e2 = BinaryExpression Multiply e1 e2
(|/|) e1 e2 = BinaryExpression Divide   e1 e2
(|^|) e1 e2 = BinaryExpression Exponent e1 e2
(|+|) e1 e2 = BinaryExpression Add      e1 e2
(|-|) e1 e2 = BinaryExpression Subtract e1 e2

printLn a = putStrLn $ show a
printExpression = putStrLn . prettify . simplify . simplify

eval :: [Statement] -> [Statement] -> IO ()
eval [] _ = return ()
eval ((DerivateStatement s):ss) statements = do
  putStrLn "derivative:"
  printExpression $ derivate (toExpression s statements)
  eval ss statements
eval ((IntegrateStatement s):ss) statements = do
  putStrLn "integral:"
  printExpression $ integrate (toExpression s statements)
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

find :: String -> [Statement] -> Statement
find s statements = head $ filter search statements
  where search (Assignment ident _) = ident == s
        search (FunctionCall (Identifier i) _) = i == s

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

toExpression :: Statement -> [Statement] -> Expression
toExpression (Calculation e) ss       = e
toExpression (Assignment i s) ss      = toExpression s ss
toExpression (DerivateStatement s) ss = derivate $ toExpression s ss
toExpression (IntegrateStatement s) ss = integrate $ toExpression s ss
toExpression (Identifier i) ss        = toExpression (find i ss) ss

derivate :: Expression -> Expression
derivate (Constant n) = Constant 0
derivate (Variable s) = Constant 1
derivate (BinaryExpression Add      e1 e2) = (derivate e1) |+| (derivate e2)
derivate (BinaryExpression Multiply (Constant c) (Variable v)) = (Constant c)
derivate (BinaryExpression Multiply e1 e2) = udv |+| vdu
  where udv = e1 |*| (derivate e2)
        vdu = (derivate e1) |*| e2
derivate (BinaryExpression Divide   e1 e2) = top |/| bottom
  where top    = ldh |-| hdl
        hdl    = e1  |*| (derivate e2)
        ldh    = e2  |*| (derivate e1)
        bottom = e2  |^| (Constant 2)
derivate (BinaryExpression Subtract e1 e2) = (derivate e1) |-| (derivate e2)
derivate e@(BinaryExpression Exponent (Constant c) (Variable v)) = (Function (Log 2) (Constant c)) |*| e
derivate (BinaryExpression Exponent e1 (Constant n)) = (Constant n) |*| (e1 |^| (Constant (n-1)))
derivate (BinaryExpression Exponent e1 e2) = (derivate e2) |*| (e1 |^| e2)


integrate :: Expression -> Expression
integrate (Constant c) = (Variable "x") |*| (Constant c)
integrate (Variable v) = ((Constant 1) |/| (Constant 2)) |*| ((Variable v) |^| (Constant 2))
integrate (BinaryExpression Exponent (Variable v) (Constant c)) = constant |*| ((Variable v) |^| (Constant (c+1)))
  where constant = (Constant 1) |/| (Constant (c+1))
integrate _ = undefined

simplify :: Expression -> Expression
simplify   (Constant n) = (Constant n)
simplify   (Variable s) = (Variable s)
simplify   (Neg (Constant 0))      = Constant 0
simplify   (Neg e)      = (Neg $ simplify e)
simplify e@(BinaryExpression binop e1 e2) = case binop of
  Add      -> simplifyAdd se1 se2
  Subtract -> simplifySub se1 se2
  Multiply -> simplifyMul se1 se2
  Divide   -> simplifyDiv se1 se2
  Exponent -> simplifyExp se1 se2
  where se1 = simplify e1
        se2 = simplify e2

simplifyAdd :: Expression -> Expression -> Expression
simplifyAdd (Constant 0) e = simplify e
simplifyAdd e (Constant 0) = simplify e
simplifyAdd (Constant a) (Constant b) = (Constant $ a + b)
simplifyAdd e1 e2 = simplify e1 |+| simplify e2

simplifyExp :: Expression -> Expression -> Expression
simplifyExp (Constant 1) _ = (Constant 1)
simplifyExp e (Constant 1) = simplify e
simplifyExp e (Constant 0) = (Constant 1)
simplifyExp (Constant 0) e = (Constant 0)
simplifyExp (Constant a) (Constant b) = (Constant $ a ^ b)
simplifyExp e1 e2 = simplify e1 |^| simplify e2

simplifyDiv :: Expression -> Expression -> Expression
simplifyDiv (Constant 0) _ = (Constant 0)
simplifyDiv _ (Constant 0) = undefined
simplifyDiv e1 e2 = simplify e1 |/| simplify e2

simplifyMul :: Expression -> Expression -> Expression
simplifyMul (Constant 0) _ = (Constant 0)
simplifyMul _ (Constant 0) = (Constant 0)
simplifyMul (Constant a) (Constant b) = (Constant $ a * b)

simplifyMul e (BinaryExpression Add e1 e2)       = (simplify e |*| simplify e1) |+| (simplify e |*| simplify e2)
simplifyMul e (BinaryExpression Subtract e1 e2)  = (simplify e |*| simplify e1) |-| (simplify e |*| simplify e2)
simplifyMul (BinaryExpression Add e1 e2)      e  = (simplify e |*| simplify e1) |+| (simplify e |*| simplify e2)
simplifyMul (BinaryExpression Subtract e1 e2) e  = (simplify e |*| simplify e1) |-| (simplify e |*| simplify e2)

simplifyMul e (BinaryExpression Multiply e1 e2)  = (simplify e |*| simplify e1) |*| simplify e2
simplifyMul (BinaryExpression Multiply e1 e2) e  = (simplify e |*| simplify e1) |*| simplify e2

simplifyMul (BinaryExpression Divide e1 e2) e  = if e == e2
  then simplify e1
  else (simplify e |*| simplify e1) |/| simplify e2
simplifyMul e (BinaryExpression Divide e1 e2)  = if e == e2
  then simplify e1
  else (simplify e |*| simplify e1) |/| simplify e2

simplifyMul e1 e2 = simplify e1 |*| simplify e2

simplifySub :: Expression -> Expression -> Expression
simplifySub (Constant 0) e = Neg $ simplify e
simplifySub e (Constant 0) = simplify e
simplifySub (Constant a) (Constant b) = (Constant $ a - b)
simplifySub e1 e2 = simplify e1 |-| simplify e2

prettify :: Expression -> String
prettify (Constant n) = show n
prettify (Variable s) = s
prettify (Neg e) = "(-" ++ prettify e ++ ")"
prettify (BinaryExpression Add        e1 e2) = prettify e1 ++ " + " ++ prettify e2
prettify (BinaryExpression Subtract   e1 e2) = prettify e1 ++ " - " ++ prettify e2
prettify (BinaryExpression Multiply  (Variable s) (Constant c)) = show c ++ s
prettify (BinaryExpression Multiply  (Constant c) (Variable s)) = show c ++ s
prettify (BinaryExpression Multiply   e1 e2) = prettify e1 ++ " * " ++ prettify e2
prettify (BinaryExpression Divide     e1 e2) = "(" ++ prettify e1 ++ ") / (" ++ prettify e2 ++ ")"
prettify (BinaryExpression Exponent   e1 e2) = prettify e1 ++ "^" ++ prettify e2
prettify _ = "meh"

getvars :: Expression -> [Expression]
getvars v@(Variable s)             = [v]
getvars (BinaryExpression _ e1 e2) = getvars e1 ++ getvars e2
getvars _                          = []


calculate :: [Statement] -> Expression -> Integer
calculate s (Constant c) = c
calculate s (Neg e) = (*) (-1) (calculate s e)
calculate s (BinaryExpression op e1 e2) = operation (calculate s e1) (calculate s e2)
  where operation = case op of
              Add      -> (+)
              Subtract -> (-)
              Multiply -> (*)
              Divide   -> div
              Exponent -> (^)
calculate statements (Variable v) = calculate statements $ toExpression (find v statements) statements
