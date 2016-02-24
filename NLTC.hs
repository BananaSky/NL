import Parser
import Eval
import Text.ParserCombinators.Parsec hiding (State)
    
main = do
    test --Parse a Hello World program
    
{-

main s: 
    print s
    
-}
    
test = do
    putStrLn cpp_code
    where cpp_code = case parse function "" "main x y:\n\tprint hello world\n\tprint goodbye world\n" of
                        Right f -> evalFunction f
                        Left _  -> undefined