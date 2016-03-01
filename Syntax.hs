module Syntax where

import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token
import           Text.Parsec

languageDef =
   emptyDef { Token.commentStart    = "{-"
            , Token.commentEnd      = "-}"
            , Token.commentLine     = "--"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = ["true", "false"]
            , Token.reservedOpNames = ["+", "-", "*", "/", "=", "&&", "||", "!"
                                     , "<", ">", "<=", ">=", "=="]
             }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace


data Expression = Variable String
                | Constant Integer
                | Neg      Expression
                | BinaryExpression BinaryOperator Expression Expression
                  deriving (Show)

data BinaryOperator = Add
                    | Subtract
                    | Multiply
                    | Divide
                      deriving (Show)

data Statement = AssignmentStatement Assignment
               | IfElseStatement IfElse
                 deriving (Show)

data IfElse = IfElse Conditional [Statement] [Statement] deriving (Show)

data Conditional = Boolean Bool
                | Not Conditional
                | Relation RelationOperator Expression Expression
                | BinaryCondition ConditionOperator Conditional Conditional
                deriving (Show)


data ConditionOperator = And | Or deriving (Show)

data RelationOperator = Greater | Less | GreaterOrEqual | LessOrEqual | Equal deriving (Show)

data Assignment = Assignment String Expression deriving (Show)
