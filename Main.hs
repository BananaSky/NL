module Main where

import Parser
import Eval
import Base
import Syntax
import Text.Parsec
import System.IO

main :: IO ()
main = do
    handle <- openFile "test.nl" ReadMode
    contents <- hGetContents handle

    let d = case parse parseDerivate "" contents of
              Right (DerivateStatement e) -> simplify e
              Left _  -> undefined



    putStrLn "y = "
    print $ prettify d
    putStrLn "y' = "
    print $ prettify $ (simplify . simplify . simplify . simplify) (derivate d)

    hClose handle

    --(endBy parseStatement whiteSpace)
