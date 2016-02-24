module Eval where

import Data.List
import Syntax

evalFunction :: Function -> String
evalFunction (Function ident args cont) = ftype ++ ident ++ arguments ++ content

        where arguments = "("   ++ (intersperseCommas args) ++ ")"
              content   = "{\n" ++ (concat $ map evalStatement cont) ++ "\n}"
              ftype     = "void "
              intersperseCommas []  = ""
              intersperseCommas [s] = s
              intersperseCommas (s:ss) = s ++ ", " ++ intersperseCommas ss
              

evalStatement :: Statement -> String
evalStatement (PrintStatement s) = "std::cout << \"" ++ s ++ "\" << std::endl;\n"