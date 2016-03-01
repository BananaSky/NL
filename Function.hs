module Function where
{-
import Text.Parsec

function :: Parser Function
function = do
name <- (many $ noneOf " :")
args <- functionArgs
char ':'
(many $ char ' ')
eol
cont <- functionContents
return $ Function name args cont

functionArgs :: Parser [Identifier]
functionArgs = do
try (parseArgs)
<|> return []
where parseArgs = do
        space
        sepBy (many $ noneOf " :") (char ' ') >>= return

functionContents :: Parser [Statement]
functionContents = do
cont <- many $ indentedLine
return $ rights $ map (parse parseStatement "") cont

indentedLine :: Parser String
indentedLine = do
        tab
        l <- line
        eol
        return l

eol =   try (string "\n\r")
<|> try (string "\r\n")
<|> string "\n"
<|> string "\r"
<?> "end of line"

-}
