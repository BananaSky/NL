module Parser where

import Control.Monad

import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Expr

import Syntax

parseStatement = try parseAssignment
             <|> parseIfElse

parseExpression :: Parser Expression
parseExpression = buildExpressionParser operators terms
  where terms =  parens parseExpression
                <|> liftM Variable identifier
                <|> liftM Constant integer

parseIfElse :: Parser Statement
parseIfElse = do
 reserved "if"
 condition <- parseConditional
 reserved "then"
 thenBranch <- many1 parseStatement
 reserved "else"
 elseBranch <- many1 parseStatement
 return $ IfElseStatement $ IfElse condition thenBranch elseBranch

parseConditional :: Parser Conditional
parseConditional = buildExpressionParser logicOperators boolean
  where boolean =  parens parseConditional
                   <|> (reserved "true"  >> return (Boolean True ))
                   <|> (reserved "false" >> return (Boolean False))
                   <|> relationExpression

relationExpression = do
   a1 <- parseExpression
   op <- relation
   a2 <- parseExpression
   return $ Relation op a1 a2
   where relation =   (reservedOp ">" >> return Greater)
                  <|> (reservedOp "<" >> return Less)
                  <|> (reservedOp ">=" >> return GreaterOrEqual)
                  <|> (reservedOp "<=" >> return LessOrEqual)
                  <|> (reservedOp "==" >> return Equal)

parseAssignment :: Parser Statement
parseAssignment = do
 identifier <- many1 letter
 space
 (char '=')
 space
 value <- parseExpression
 return $ AssignmentStatement $ Assignment identifier value
