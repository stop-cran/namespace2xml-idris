module Main

import CommandLineArguments
import ConfigData
import ConfigParser
import ConfigReader
import ConfigTransform
import System
import MonadSwap


printHelp : IO ()
printHelp = putStrLn """-i <a list of input files>
-s <a list of schema files>
-o <an output directory>"""

printOverrideMessage : ConfigLineOverride -> IO ()
printOverrideMessage (MkOverride (Payload _ _ file line) (Payload key _ newFile newLine)) =
    putStrLn $ "Entry \"" ++ (formatName key) ++ "\" from \"" ++ file ++ "\", line " ++ show line ++
        " has been overridden at \"" ++ newFile ++ "\", line " ++ show newLine ++ "."

printOverrideMessages : List ConfigLineOverride -> IO ()
printOverrideMessages = foldr (*>) (pure ()) . map printOverrideMessage

joinC : List (IO (Maybe (List a))) -> IO (Maybe (List a))
joinC = map (map join . swap) . swap

join' : List (IO (List a)) -> IO (List a)
join' = map join . swap

process : (input : List String) -> (schema : List String) -> (outputDir : String) -> IO ()
process input schema outputDir = do
    putStrLn $ "input: " ++ (show input)
    putStrLn $ "schema: " ++ (show schema)
    putStrLn $ "output: " ++ (show outputDir)
    inputProcessingResult <- joinC $ map readConfigFromFile input
    case inputProcessingResult of
        Nothing => do
            putStrLn "Exiting (error)..."
            exit 1
        Just result => let (overridden, old) = overrideConfig result in do
            printOverrideMessages old
            success <- writeConfigToFile (outputDir ++ "/output.txt") overridden


            case success of
                False => do
                    putStrLn "Exiting (error)..."
                    exit 1
                True => putStrLn "Exiting..."

main : IO ()
main = do
    args <- getArguments
    case args of
        Payload input schema outputDir => process input schema outputDir
        Help => printHelp
        Invalid message => do
            putStrLn "Invalid command line arguments: "
            putStrLn message
            putStrLn "Should be:"
            printHelp
            exit 1
