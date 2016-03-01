module Calc(parseExpression) where

import           Syntax

import           Control.Monad

import           Text.Parsec
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Expr

--Calculations Module

parseExpression :: Parser Expression
parseExpression = buildExpressionParser operators terms

operators = [  [Prefix  (reservedOp "-"   >> return (Neg             ))          ]
            , [Infix  (reservedOp "*"   >> return (BinaryExpression Multiply)) AssocLeft,
               Infix  (reservedOp "/"   >> return (BinaryExpression Divide  )) AssocLeft]
            , [Infix  (reservedOp "+"   >> return (BinaryExpression Add     )) AssocLeft,
               Infix  (reservedOp "-"   >> return (BinaryExpression Subtract)) AssocLeft]
             ]

terms =  parens parseExpression
    <|> liftM Variable identifier
    <|> liftM Constant integer
