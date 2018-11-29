module CommandLineArguments

import CommandLineParser
import Lightyear
import Lightyear.Core
import Lightyear.Strings
import System


public export
data Arguments : Type where
    Payload: (input : List String) -> (schema : List String) -> (outputDir : String) -> Arguments
    Help : Arguments
    Invalid : (message : String) -> Arguments

flatten : Either String Arguments -> Arguments
flatten (Left message) = Invalid message
flatten (Right input) = input

helpParser : Monad m => ParserT (List String) m Arguments
helpParser = (key "-h" <|> key "-?" <|> key "--help") *> pure Help <?> "help"

payloadParser : Monad m => ParserT (List String) m Arguments
payloadParser = do
    key "-i"
    input     <- manyTill anyToken (key "-s") <?> "a list of input files"
    schema    <- manyTill anyToken (key "-o") <?> "a list of schema files"
    outputDir <- anyToken <?> "an output directory"
    eof
    pure $ Payload input schema outputDir

export
getArguments : IO Arguments
getArguments = do
    (_ :: args) <- getArgs
    pure $ flatten $ parseList (helpParser <|> payloadParser) args