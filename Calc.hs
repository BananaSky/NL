module Calc (calculate) where
--Module for evaluating mathamatical functions

import Text.ParserCombinators.Parsec
import Control.Monad

data Expression = Constant Int
                | Addition       Expression Expression
                | Subtraction    Expression Expression
                | Multiplication Expression Expression
                | Division       Expression Expression
                deriving (Show)
              
calculate :: String -> Maybe Int
calculate s = calculated
    where calculated = case parse parseExpr "" s of
                Right e  -> Just (interpretExpression e)
                Left  _  -> Nothing
                
parseExpr :: GenParser Char st Expression
parseExpr = do  try (operator '+')
            <|> try (operator '-')
            <|> try (operator '*')
            <|> try (operator '/')
            <|> term
                  
term :: GenParser Char st Expression
term = do
    t <- (many $ noneOf " +-*/")
    return  $ Constant (read t :: Int)
    
operator :: Char -> GenParser Char st Expression
operator c = do
    t1 <- term
    space
    char c
    space
    t2 <- term
    return $ case c of
        '+' -> Addition t1 t2
        '-' -> Subtraction t1 t2
        '/' -> Division t1 t2
        '*' -> Multiplication t1 t2 
        
                
interpretExpression :: Expression -> Int
interpretExpression (Constant n) = n
interpretExpression (Addition       e1 e2) = (interpretExpression e1)   +   (interpretExpression e2)
interpretExpression (Subtraction    e1 e2) = (interpretExpression e1)   -   (interpretExpression e2)
interpretExpression (Multiplication e1 e2) = (interpretExpression e1)   *   (interpretExpression e2)
interpretExpression (Division       e1 e2) = (interpretExpression e1) `div` (interpretExpression e2)