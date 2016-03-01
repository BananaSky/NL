module Conditional(parseIfElse, parseConditional) where

import Syntax
import Calc

import           Text.Parsec
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Expr

--Conditional Module

logicOperators = [ [Prefix (reservedOp "!"  >> return (Not             ))          ]
                ,  [Infix  (reservedOp "&&" >> return (BinaryCondition And     )) AssocLeft,
                    Infix  (reservedOp "||" >> return (BinaryCondition Or      )) AssocLeft]
             ]

parseIfElse :: Parser Statement
parseIfElse = undefined {-
  do
  reserved "if"
  condition <- parseConditional
  reserved "then"
  thenBranch <- many1 parseStatement
  reserved "else"
  elseBranch <- many1 parseStatement
  return $ IfElseStatement $ IfElse condition thenBranch elseBranch -}


parseConditional :: Parser Conditional
parseConditional = buildExpressionParser logicOperators boolean

boolean =  parens parseConditional
     <|> (reserved "true"  >> return (Boolean True ))
     <|> (reserved "false" >> return (Boolean False))
     <|> relationExpression

relationExpression =
  do a1 <- parseExpression
     op <- relation
     a2 <- parseExpression
     return $ Relation op a1 a2
     where relation =   (reservedOp ">" >> return Greater)
                    <|> (reservedOp "<" >> return Less)
                    <|> (reservedOp ">=" >> return GreaterOrEqual)
                    <|> (reservedOp "<=" >> return LessOrEqual)
                    <|> (reservedOp "==" >> return Equal)
