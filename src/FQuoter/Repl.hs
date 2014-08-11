module FQuoter.Repl
(shellForNotDefined)
where

import Control.Applicative
import System.Console.Haskeline

import FQuoter.Quote
import FQuoter.Parser.ParserTypes

shellForNotDefined :: NotDefinedType -> InputT IO ParsedType
shellForNotDefined (NDAuthor) = loopAuthor

ask :: String -> InputT IO String
ask q = outputStrLn q >> getInputLine "> " >>= return . nothingToEmpty

ask' :: String -> InputT IO (Maybe String)
ask' q = outputStrLn q >> getInputLine "> " >>= return . emptyToNothing

nothingToEmpty Nothing   = ""
nothingToEmpty (Just s)  = s

emptyToNothing :: Maybe String -> Maybe String
emptyToNothing Nothing   = Nothing
emptyToNothing (Just "") = Nothing
emptyToNothing (Just s)  = Just s

loopAuthor :: InputT IO ParsedType
loopAuthor = readAuthor >>= return . PAuthor
    where readAuthor = Author <$> ask' "Enter the first name"
                              <*> ask' "Enter the last name"
                              <*> ask' "Enter the nick name if any"
