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
            , Token.reservedNames   = ["true", "false", "if", "then", "else", "print", "return"]
            , Token.reservedOpNames = ["+", "-", "*", "/", "=", "and", "or", "not"
                                     , "<", ">", "<=", ">=", "==", ":"]
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

--Calculation

data Expression = Variable String
                | Constant Integer
                | Neg      Expression
                | BinaryExpression BinaryOperator Expression Expression
                | Function LibraryFunction Expression
                  deriving (Eq, Show)

data BinaryOperator = Add
                    | Subtract
                    | Multiply
                    | Divide
                    | Exponent
                      deriving (Eq, Show)

type Base = Int
data LibraryFunction = Log Int
                     | Sin
                     | Cos
                     | Tan
                     deriving (Eq, Show)

data Type = Number   Int
          deriving (Eq, Show)

--AST

data Statement = Assignment          String Statement
               | FunctionCall        Statement [Type]
               | DerivateStatement   Statement
               | Identifier          String
               | Calculation         Expression
               | None
                 deriving (Eq, Show)
