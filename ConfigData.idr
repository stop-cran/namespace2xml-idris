module ConfigData

import Data.NEList
import NEListExtras

%access public export


data NameToken = TextNameToken String | SubstituteNameToken

data ValueToken : Type where
    TextValueToken : String -> ValueToken
    ReferenceValueToken : NEList (NEList NameToken) -> ValueToken

data ConfigLine : Type where
    Payload : (name : NEList (NEList NameToken)) -> (value : List ValueToken) -> (file: String) -> (lineNo: Nat) -> ConfigLine
    Comment : String -> ConfigLine

intercalate : String -> List String -> String
intercalate s [] = ""
intercalate s [x] = x
intercalate s (x :: xs) = x ++ s ++ intercalate s xs

Show NameToken where
    show (TextNameToken s) = s
    show SubstituteNameToken = "*"

isSubstitute : NameToken -> Bool
isSubstitute SubstituteNameToken = True
isSubstitute _ = False

Eq NameToken where
    (TextNameToken x) == (TextNameToken y) = x == y
    _ == _ = False

Eq ValueToken where
    (TextValueToken x) == (TextValueToken y) = x == y
    (ReferenceValueToken x) == (ReferenceValueToken y) = x == y
    _ == _ = False

formatName : NEList (NEList NameToken) -> String
formatName l = intercalate "." $ Data.NEList.toList $ map (intercalate "" . Data.NEList.toList . map show) l

Show ValueToken where
    show (TextValueToken s) = s
    show (ReferenceValueToken r) = "${" ++ formatName r ++ "}"

Show ConfigLine where
    show (Payload name value _ _) = formatName name ++ "=" ++ (intercalate "" . map show $ value) -- (show lineNo) ++ ". " ++ ...
    show (Comment comment) = "# " ++ comment

formatConfig : List ConfigLine -> String
formatConfig c = intercalate "\n" . map show $ c