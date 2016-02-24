module Syntax where

type Identifier = String
data Function  = Function Identifier [Identifier] [Statement] deriving (Show)

data Statement = PrintStatement String 
                 deriving (Show)