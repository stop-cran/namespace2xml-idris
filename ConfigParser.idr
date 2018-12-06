module ConfigParser

import Control.Monad.State
import TParsec
import TParsec.Running
import ConfigData
import Data.NEList
import NEListExtras
import TParsecExtras

%default total
%access public export


nameChar : Parser' Char
nameChar = (char '\\' `rand` anyCharOf "\\*.}") `alt` noneCharOf "*.=}\r\n"

textNameToken : Parser' NameToken
textNameToken = map (TextNameToken . nepack) (nelist nameChar)

substituteNameToken : Parser' NameToken
substituteNameToken = cmap SubstituteNameToken (char '*')

name : Parser' (NEList (NEList NameToken))
name = sepBy (nelist (substituteNameToken `alt` textNameToken)) (char '.')

valueChar : Parser' Char
valueChar = guardM id $ cmap Nothing (string "${") `alt` map Just (noneCharOf "*.}\r\n")

textValueToken : Parser' ValueToken
textValueToken = map (TextValueToken . nepack) (nelist valueChar)

referenceValueToken : Parser' ValueToken
referenceValueToken = (string "${" `rand` map ReferenceValueToken name) `land` char '}'

value : Parser' (NEList ValueToken)
value = nelist $ referenceValueToken `alt` textValueToken

payload : Parser' ConfigLine
payload = map (\(name, val) => Payload name (toListMaybe val) "" 0) $
          (name `land` char '=') `andopt` value

comment : Parser' ConfigLine
comment = ((spaces `optand` char '#') `andopt` spaces) `rand`
          map (Comment . nepack) (nelist (noneCharOf "\r\n"))

eol : Parser' ()
eol = cmap () $ char '\r' `optand` exactChar '\n'

eolWithSpaces : Parser' ()
eolWithSpaces = cmap () $ nelist $ spaces `optand` eol

config : Parser' (List ConfigLine)
config = map Data.NEList.toList $
    (eolWithSpaces `roptand` sepBy (comment `alt` payload) eolWithSpaces) `landopt` eolWithSpaces
