module ConfigParser

import ConfigData
import Lightyear
import Lightyear.Position
import Lightyear.Core
import Lightyear.Char
import Lightyear.Strings


mutual
    someTillFail : Parser a -> Parser (List a)
    someTillFail p = do
        x <- p
        (x ::) <$> manyTillFail p

    manyTillFail : Parser a -> Parser (List a)
    manyTillFail p = someTillFail p <|> pure []

someCharsTillFail : Parser Char -> Parser String
someCharsTillFail = map pack . someTillFail

whiteSpace : Parser ()
whiteSpace = skip $ many $ oneOf " \t"

endOfLineWithSpaces : Parser Char
endOfLineWithSpaces = whiteSpace *> endOfLine


-- Name

nameChar : Parser Char
nameChar = char '\\' *> oneOf "\\*.}" <|> noneOf "*.=}\r\n"

textNameToken : Parser NameToken
textNameToken = TextNameToken <$> someCharsTillFail nameChar

substituteNameToken : Parser NameToken
substituteNameToken = char '*' *> pure SubstituteNameToken

name : Parser (List NameToken)
name = someTillFail (substituteNameToken <|> textNameToken) <?> "a name"

qualifiedName : Parser (List (List NameToken))
qualifiedName = sepBy1 name $ char '.' <?> "a qualified name"


-- Value

valueChar : Parser Char
valueChar = requireFailure (string "${") *> noneOf "\r\n"

textValueToken : Parser ValueToken
textValueToken = TextValueToken <$> someCharsTillFail valueChar <?> "a static value"

referenceValueToken : Parser ValueToken
referenceValueToken = string "${" *> map ReferenceValueToken qualifiedName <* char '}' <?> "a reference"

value : Parser (List ValueToken)
value = manyTillFail $ textValueToken <|> referenceValueToken <?> "a value"


-- Config

payload : Parser ConfigLine
payload = (do
    name <- qualifiedName
    position <- getPosition
    char '='
    val <- value
    pure $ Payload name val "" $ lineNo position) <?> "a key-value pair"

comment : Parser ConfigLine
comment = whiteSpace *> char '#' *> whiteSpace *>
          Comment <$> someCharsTillFail (requireFailure endOfLineWithSpaces *> anyChar) <?> "a comment"

export
config : Parser (List ConfigLine)
config = many endOfLineWithSpaces *> sepBy (comment <|> payload) (some endOfLineWithSpaces) <*
         many endOfLineWithSpaces <* eof
