module FQuoter.Repl
(shellForNotDefined)
where

import Control.Applicative
import Control.Monad
import System.Console.Haskeline
import qualified Data.Map as Map

import FQuoter.Quote
import FQuoter.Parser.ParserTypes

prompt = "> "

shellForNotDefined :: NotDefinedType -> InputT IO ParsedType
shellForNotDefined (NDAuthor) = loopAuthor
shellForNotDefined (NDSource) = loopSource
shellForNotDefined (NDQuote) = loopQuote

getInputLine' :: String -> InputT IO String
getInputLine' s = getInputLine s >>= return . nothingToEmpty

nothingToEmpty Nothing   = ""
nothingToEmpty (Just s)  = s

emptyToNothing :: Maybe String -> Maybe String
emptyToNothing Nothing   = Nothing
emptyToNothing (Just "") = Nothing
emptyToNothing (Just s)  = Just s

ask :: String -> InputT IO String
ask q = outputStrLn q >> getInputLine' prompt

ask' :: String -> InputT IO (Maybe String)
ask' q = outputStrLn q >> getInputLine prompt >>= return . emptyToNothing

loopAsk :: String -> InputT IO [String]
loopAsk q = do outputStrLn q
               i <- getInputLine' prompt
               if null i
                    then return []
                    else (i :) <$> (loopAsk q)

loopDict :: String -> String -> InputT IO (Map.Map String String)
loopDict q1 q2 = loopDict' >>= return . Map.fromList 
    where
        loopDict' = do outputStrLn q1
                       k <- getInputLine' prompt
                       if null k
                        then return []
                        else do outputStrLn q2
                                v <- getInputLine' prompt 
                                ( (k,v) :) <$> (loopDict')

loopAuthor :: InputT IO ParsedType
loopAuthor = readAuthor >>= return . PAuthor
    where readAuthor = Author <$> ask' "Enter the first name"
                              <*> ask' "Enter the last name"
                              <*> ask' "Enter the nick name if any"

loopSource :: InputT IO ParsedType
loopSource = readSource >>= return . PSource
    where readSource = ParserSource <$> ask "Enter the title"
                       <*> loopAsk "Enter the author(s)"
                       <*> loopDict "Enter a metadata type" "Enter a metadata value"

loopQuote :: InputT IO ParsedType
loopQuote = readQuote >>= return . PQuote
    where readQuote = ParserQuote <$> ask "Enter the quote"
                                  <*> ask "Enter the source"
                                  <*> ask' "Enter localization if any"
                                  <*> loopAsk "Enter tags. Empty input to stop entering tag."
                                  <*> loopAsk "Enter author. Empty input to stop entering tag."
                                  <*> ask' "Enter a comment if you have one"
