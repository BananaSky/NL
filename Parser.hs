module Parser where

import Text.ParserCombinators.Parsec
import Syntax
import Data.Either
    
file = do
    many $ function
    eof
    
function :: GenParser Char st Function
function = do
    name <- (many $ noneOf " :")
    args <- functionArgs
    char ':'
    (many $ char ' ')
    eol
    cont <- functionContents
    return $ Function name args cont
    
functionArgs :: GenParser Char st [Identifier]
functionArgs = do
    try (parseArgs)
    <|> return []
    where parseArgs = do
            space
            sepBy (many $ noneOf " :") (char ' ') >>= return
    
functionContents :: GenParser Char st [Statement]
functionContents = do
    cont <- many $ indentedLine
    return $ rights $ map (parse parseStatement "") cont
    
indentedLine :: GenParser Char st String
indentedLine = do
            tab 
            l <- line
            eol
            return l  
           
line :: GenParser Char st String
line = many $ noneOf "\n\r"

parseStatement :: GenParser Char st Statement
parseStatement = do
    parsePrint
    <?> "Statement Parse Attempt failed"
           
parsePrint :: GenParser Char st Statement
parsePrint = do
    string "print"
    space
    l <- line
    return $ PrintStatement l
    
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
    