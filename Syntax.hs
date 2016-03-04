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
            , Token.reservedNames   = ["true", "false", "if", "then", "else", "print"]
            , Token.reservedOpNames = ["+", "-", "*", "/", "=", "and", "or", "not"
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


operators = [  [Prefix  (reservedOp "-"   >> return (Neg             ))          ]
            , [Infix  (reservedOp "^"   >> return (BinaryExpression Exponent)) AssocLeft,
               Infix  (reservedOp "*"   >> return (BinaryExpression Multiply)) AssocLeft,
               Infix  (reservedOp "/"   >> return (BinaryExpression Divide  )) AssocLeft]
            , [Infix  (reservedOp "+"   >> return (BinaryExpression Add     )) AssocLeft,
               Infix  (reservedOp "-"   >> return (BinaryExpression Subtract)) AssocLeft]
             ]


logicOperators = [ [Prefix (reservedOp "not"  >> return (Not             ))          ]
               ,  [Infix  (reservedOp "and" >> return (BinaryCondition And     )) AssocLeft,
                   Infix  (reservedOp "or" >> return (BinaryCondition Or      )) AssocLeft]
            ]

--Calculation

data Expression = Variable String
                | Constant Integer
                | Neg      Expression
                | BinaryExpression BinaryOperator Expression Expression
                  deriving (Show)

data BinaryOperator = Add
                    | Subtract
                    | Multiply
                    | Divide
                    | Exponent
                      deriving (Show)

--Conditional

data IfElse = IfElse Conditional [Statement] [Statement] deriving (Show)

data Conditional = Boolean Bool
                | Not Conditional
                | Relation RelationOperator Expression Expression
                | BinaryCondition ConditionOperator Conditional Conditional
                deriving (Show)


data ConditionOperator = And | Or deriving (Show)

data RelationOperator = Greater | Less | GreaterOrEqual | LessOrEqual | Equal deriving (Show)

--AST (ish)

data Assignment = Assignment String Expression deriving (Show)

data Function = Function String [String] [Statement] deriving (Show)

data Statement = AssignmentStatement Assignment
               | IfElseStatement IfElse
               | FunctionStatement Function
               | PrintStatement Statement
               | RawType Type
                 deriving (Show)

data Type = Sentence String
          | Number   Int
          deriving (Show)
