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
    content <- hGetContents handle
    let contents = lines content

    let statements = parseFile contents []
    mapM printLn statements

    putStrLn ""

    eval statements statements

    hClose handle

    let e = (Variable "x" |^| Constant 3) |*| ((Variable "x" |^| Constant 4 |+| Constant 1) |^| (Constant 2))
    let u = (Variable "x" |^| Constant 4 |+| Constant 1)

    --print $ prettify e

    --print $ prettify $ integrate e

    return ()
