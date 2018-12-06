module ConfigReader

import ConfigData
import ConfigParser
import Data.NEList
import NEListExtras
import TParsec
import TParsec.Running


showLeft: Show a => Either a b -> Either String b
showLeft (Left x) = Left $ show x
showLeft (Right x) = Right x

infixl 10 ?++
export
(?++): Either String a -> String -> Either String a
(Left message) ?++ comment = Left $ comment ++ ": " ++ message
right ?++ _ = right

withMessage : Maybe a -> String -> Either String a
withMessage Nothing message = Left message
withMessage (Just x) _ = Right x

withFile: String -> ConfigLine -> ConfigLine
withFile _ c@(Comment _) = c
withFile file (Payload name value _ line) = Payload name value file line

export
readConfigFromFile : String -> IO (Maybe (List ConfigLine))
readConfigFromFile file = do
    putStrLn $ "Reading " ++ file ++ "..."
    content <- readFile file
    let inputProcessingResult = do
        content <- (showLeft content) ?++ "Error reading an input"
        parseResult <- withMessage (parseMaybe content config) "Error parsing an input"
        pure parseResult
    case inputProcessingResult ?++ file of
        Left error => do
            putStrLn error
            pure Nothing
        Right result => pure $ Just $ map (withFile file) result


export
writeConfigToFile : String -> List ConfigLine -> IO Bool
writeConfigToFile file config = do
    putStrLn "Writing \"output.txt\"..."
    writeResult <- writeFile file (formatConfig config)
    case showLeft writeResult ?++ "Error writing output" of
        Left error => do
            putStrLn error
            pure False
        Right () => do
            putStrLn "Success!"
            pure True