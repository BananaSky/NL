module Math where

import Syntax

infixl 7 |*|
infixl 7 |/|
infixl 8 |^|
infixl 6 |+|
infixl 6 |-|
infixl 5 |=|

(|*|) e1 e2 = BinaryExpression Multiply e1 e2
(|/|) e1 e2 = BinaryExpression Divide   e1 e2
(|^|) e1 e2 = BinaryExpression Exponent e1 e2
(|+|) e1 e2 = BinaryExpression Add      e1 e2
(|-|) e1 e2 = BinaryExpression Subtract e1 e2
(|=|) e1 e2 = Equality e1 e2

fromOp op = case op of
            Add      -> (+)
            Subtract -> (-)
            Multiply -> (*)
            Divide   -> div
            Exponent -> (^)

fromOp' op = case op of
            Add      -> (|+|)
            Subtract -> (|-|)
            Multiply -> (|*|)
            Divide   -> (|/|)
            Exponent -> (|^|)

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
simplify (Equality e1 e2) = if isConstant e1 then simplify e2 |=| simplify e1
                                             else simplify e1 |=| simplify e2
simplify (Function f e) = (Function f (simplify e))
--simplify e = e

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
simplifyMul (Constant 1) e = e
simplifyMul e (Constant 1) = e
simplifyMul (Constant a) (Constant b) = (Constant $ a * b)

simplifyMul e (BinaryExpression Add e1 e2)       = simplify $ (simplify e |*| simplify e1) |+| (simplify e |*| simplify e2)
simplifyMul e (BinaryExpression Subtract e1 e2)  = simplify $ (simplify e |*| simplify e1) |-| (simplify e |*| simplify e2)
simplifyMul (BinaryExpression Add e1 e2)      e  = simplify $ (simplify e |*| simplify e1) |+| (simplify e |*| simplify e2)
simplifyMul (BinaryExpression Subtract e1 e2) e  = simplify $ (simplify e |*| simplify e1) |-| (simplify e |*| simplify e2)

simplifyMul e (BinaryExpression Multiply e1 e2)  = (simplify e |*| simplify e1) |*| simplify e2
simplifyMul (BinaryExpression Multiply e1 e2) e  = (simplify e |*| simplify e1) |*| simplify e2

simplifyMul (BinaryExpression Divide e1 e2) e  = if simplify e == simplify e2
  then simplify e1
  else (simplify e |*| simplify e1) |/| simplify e2
simplifyMul e (BinaryExpression Divide e1 e2)  = if simplify e == simplify e2
  then simplify e1
  else (simplify e |*| simplify e1) |/| simplify e2

simplifyMul e1 e2 = simplify e1 |*| simplify e2

simplifySub :: Expression -> Expression -> Expression
simplifySub (Constant 0) e = Neg $ simplify e
simplifySub e (Constant 0) = simplify e
simplifySub (Constant a) (Constant b) = (Constant $ a - b)
simplifySub e1 e2 = simplify e1 |-| simplify e2

calculate :: [Statement] -> Expression -> Integer
calculate s (Constant c) = c
calculate s (Neg e) = (*) (-1) (calculate s e)
calculate s (BinaryExpression op e1 e2) = (fromOp op) (calculate s e1) (calculate s e2)
calculate statements (Variable v) = calculate statements $ toExpression (find v statements) statements

toExpression :: Statement -> [Statement] -> Expression
toExpression (Calculation e) ss        = e
toExpression (Assignment i s) ss       = toExpression s ss
toExpression (DerivateStatement s) ss  = derivate $ toExpression s ss
toExpression (IntegrateStatement s) ss = integrate $ toExpression s ss
toExpression (Identifier i) ss         = toExpression (find i ss) ss

derivate :: Expression -> Expression
derivate (Constant n) = Constant 0
derivate (Variable s) = Constant 1
derivate (Neg e)      = (Neg $ derivate e)
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
derivate e@(BinaryExpression Exponent (Constant c) (Variable v)) = (Function (Log (Fractional 2.71)) (Constant c)) |*| e
derivate (BinaryExpression Exponent e1 (Constant n)) = (Constant n) |*| (e1 |^| (Constant (n-1)))
derivate (BinaryExpression Exponent e1 e2) = (derivate e2) |*| (e1 |^| e2)
derivate (Function (Log b) e) = ((Constant 1) |/| e) |*| derivate e
derivate (Function Sin e) = derivate e |*| (Function Cos e)
derivate (Function Cos e) = Neg $ derivate e |*| (Function Sin e)
derivate (Function Tan e) = derivate e |*| (Function Sec e |^| Constant 2)
derivate (Function Sec e) = derivate e |*| (Function Sec e |*| Function Tan e)
derivate (Function Csc e) = derivate e |*| ((Neg (Function Csc e)) |+| (Function Cot e))
derivate (Function Cot e) = derivate e |*| (Neg ((Function Csc e) |^| (Constant 2)))

--Derivate sans chain rule
nochain :: Expression -> Expression
nochain (Constant n) = Constant 0
nochain (Variable s) = Constant 1
nochain (Neg e)      = (Neg $ nochain e)
nochain (BinaryExpression Add      e1 e2) = (nochain e1) |+| (nochain e2)
nochain (BinaryExpression Multiply (Constant c) (Variable v)) = (Constant c)
nochain (BinaryExpression Multiply e1 e2) = udv |+| vdu
  where udv = e1 |*| (nochain e2)
        vdu = (nochain e1) |*| e2
nochain (BinaryExpression Divide   e1 e2) = top |/| bottom
  where top    = ldh |-| hdl
        hdl    = e1  |*| (nochain e2)
        ldh    = e2  |*| (nochain e1)
        bottom = e2  |^| (Constant 2)
nochain (BinaryExpression Subtract e1 e2) = (nochain e1) |-| (nochain e2)
nochain e@(BinaryExpression Exponent (Constant c) (Variable v)) = (Function (Log (Fractional 2.71)) (Constant c)) |*| e
nochain (BinaryExpression Exponent e1 (Constant n)) = e1 |^| (Constant (n-1))
nochain (BinaryExpression Exponent e1 e2) = e1 |^| e2
nochain (Function (Log b) e) = (Constant 1) |/| e
nochain (Function Sin e) = Function Cos e
nochain (Function Cos e) = Neg $ Function Sin e

integrate :: Expression -> Expression
integrate (Constant c) = (Variable "x") |*| (Constant c)
integrate (Variable v) = ((Constant 1) |/| (Constant 2)) |*| ((Variable v) |^| (Constant 2))
integrate (Neg e)      = (Neg $ integrate e)
integrate (BinaryExpression Exponent ex (Constant c)) = constant |*| (ex |^| (Constant (c+1)))
  where constant = (Constant 1) |/| (Constant (c+1))
integrate (BinaryExpression Add      e1 e2) = integrate e1 |+| integrate e2
integrate (BinaryExpression Subtract e1 e2) = integrate e1 |-| integrate e2
integrate (BinaryExpression Multiply (Constant c) e2) = (Constant c) |*| integrate e2
integrate (BinaryExpression Multiply e1 (Constant c)) = (Constant c) |*| integrate e1
integrate (BinaryExpression Divide (Constant 1) (Variable v)) = (Function (Log (Fractional 2.71)) (Variable v))
integrate (Function Sin (Variable x)) = Neg (Function Cos (Variable x)) 
integrate (Function Cos (Variable x)) = (Function Sin (Variable x))
integrate e = if (length $ filter (testUSub e) (terms e)) > 0
              then uSub e
              else byParts e

uSub :: Expression -> Expression
uSub e = constantPart |*| integrate (simplify $ remove e du)
  where possible_us  = filter (testUSub e) ts
        u            = head $ possible_us
        ts           = terms e
        du           = simplify . simplify $ nochain u
        constantPart = solve (remove u du |=| u)

testUSub :: Expression -> Expression -> Bool
testUSub e u = if u /= e then isConstant $  remove (remove e du) u
           else False
  where du = simplify .simplify $ nochain u --Derivate, but don't apply chain rule

byParts :: Expression -> Expression
byParts e = (u |*| v) |-| integrate (v |*| du)
  where ts          = terms e
        possible_us = filter (testByParts e) ts
        u           = head $ possible_us
        dv          = remove e u
        du          = simplify . simplify $ derivate u
        v           = integrate dv

testByParts :: Expression -> Expression -> Bool
testByParts e u = isConstant $ simplify . simplify $ derivate (remove e u)

find :: String -> [Statement] -> Statement
find s statements = head $ filter search statements
  where search (Assignment i _)                = i == s
        search (FunctionCall (Identifier i) _) = i == s
        search _ = False

terms :: Expression -> [Expression]
terms ex = if nonConst ex
then case ex of
  (Neg e)                      -> [ex] ++ terms e
  (BinaryExpression op e1 e2)  -> [ex] ++ terms e1 ++ terms e2
  (Function _ e)               -> [ex] ++ terms e
  e                            -> [e]
else []

nonConst e = not $ isConstant e

isConstant :: Expression -> Bool
isConstant (Constant c)               = True
isConstant (Neg e)                    = isConstant e
isConstant (BinaryExpression _ e1 e2) = isConstant e1 && isConstant e2
isConstant (Function _ e)             = isConstant e
isConstant (Variable _)               = False

isVariable :: Expression -> Bool
isVariable (Variable _) = True
isVariable _            = False

solve :: Expression -> Expression
solve (Equality e1 e2)
  | isConstant e1 && not (isConstant e2) = isolate e1 e2
  | not (isConstant e1) && isConstant e2 = isolate e2 e1
  | otherwise = undefined

isolate :: Expression -> Expression -> Expression
isolate constantPart v@(Variable _) = v |=| constantPart
isolate constantPart variantPart =
  case variantPart of
    (Neg e)                     -> solve $ (Neg constantPart) |=| e
    (Function f e)              -> solve $ (Function (reverseFunction f) constantPart) |=| e
    (BinaryExpression Exponent e1 e2) -> isolateExponent
      where isolateExponent
              | isConstant e1 && isVariable e2 = (Function (Log  e1) constantPart) |=| e2
              | isVariable e1 && isConstant e2 = (Function (Root e2) constantPart) |=| e1
              | isConstant e1 && not (isConstant e2) = solve $ (Function (Log  e1) constantPart) |=| e2
              | isConstant e2 && not (isConstant e1) = solve $ (Function (Root e2) constantPart) |=| e1
    (BinaryExpression op e1 e2) -> isolateBinary
      where isolateBinary
              | isConstant e1 && isVariable e2 = (BinaryExpression (reverseOp op) constantPart e1) |=| e2
              | isVariable e1 && isConstant e2 = (BinaryExpression (reverseOp op) constantPart e2) |=| e1
              | isConstant e1 && not (isConstant e2) = solve $ (BinaryExpression (reverseOp op) constantPart e1) |=| e2
              | isConstant e2 && not (isConstant e1) = solve $ (BinaryExpression (reverseOp op) constantPart e2) |=| e1
              | otherwise = undefined

reverseFunction :: LibraryFunction -> LibraryFunction
reverseFunction func = undefined --case func of

reverseOp :: BinaryOperator -> BinaryOperator
reverseOp op = case op of
  Add      -> Subtract
  Subtract -> Add
  Multiply -> Divide
  Divide   -> Multiply


remove :: Expression -> Expression -> Expression
remove e removal = if e == removal then Constant 1
  else case e of
    (Neg e1)                           -> Neg $ remove e1 removal
    (BinaryExpression Multiply e1 e2)  -> (remove e1 removal) |*| (remove e2 removal)
    (BinaryExpression Exponent e1 e2)  -> (remove e1 removal) |^| (remove e2 removal)
    (Function f e1)                    -> (Function f $ remove e1 removal)
    e                                  -> e
