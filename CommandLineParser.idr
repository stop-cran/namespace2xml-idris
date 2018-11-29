module CommandLineParser

import Lightyear
import Lightyear.Combinators
import Lightyear.Core
import Lightyear.Char
import Lightyear.Position
import Lightyear.Strings

%access public export


implementation Stream String (List String) where
    uncons Nil       = Nothing
    uncons (x :: xs) = Just (x, xs)
    updatePos tw pos _ = (Lightyear.Position.Generic.increment tw pos ' ', pos)

||| RK: Taken from Lightyear.
||| Run some parser `p` until the second parser is encountered,
||| collecting a list of success for `p`, and the result of the second
||| parser is dropped.
|||
||| Primarily useful for collecting single line comments and other
||| similar verbatim environments.
manyTill : Monad m => ParserT str m a
                   -> ParserT str m b
                   -> ParserT str m (List a)
manyTill p end = scan where
    scan : Monad m => ParserT str m (List a)
    scan = do { end; pure List.Nil } <|>
           do { x <- p; xs <- scan; pure (x::xs)}

||| A command line key
key : Monad m => String -> ParserT (List String) m String
key s = satisfy (== s)

||| A typed value with given string parser
value : Monad m => Parser a -> ParserT (List String) m a
value valueParser = (do
    t <- anyToken
    forward $ parse valueParser t) <?> "a typed value" where
        forward (Left error)   = fail error
        forward (Right result) = pure result

||| Run a parser against an input string assuming that tab-widths are
||| eight characters in length.
parseList : ParserT (List String) (Either String) a -> List String -> Either String a
parseList parser args = do
    state <- execParserT parser (ST args (MkPos Nothing 0 0) 8)
    case state of
        (MkReply _ (Success result)) => Right result
        (MkReply _ (Failure error))  => Left $ concat $ intersperse "\n" $ map display error
