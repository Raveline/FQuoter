-- Various utilities using Haskeline
module FQuoter.Repl.ReplUtils
(
-- Simple constant for prompt display
prompt
-- GetInputLine without Maybe result
,getInputLine'
-- Simple prompt + get line
,ask
-- Prompt + get line, return Nothing if empty input
,ask')
where

import System.Console.Haskeline
import Control.Monad (liftM)

prompt = "> "

getInputLine' :: String -> InputT IO String
getInputLine' s = liftM nothingToEmpty (getInputLine s) 

nothingToEmpty Nothing   = ""
nothingToEmpty (Just s)  = s

emptyToNothing :: Maybe String -> Maybe String
emptyToNothing Nothing   = Nothing
emptyToNothing (Just "") = Nothing
emptyToNothing (Just s)  = Just s

ask :: String -> InputT IO String
ask q = outputStrLn q >> getInputLine' prompt

ask' :: String -> InputT IO (Maybe String)
ask' q = liftM emptyToNothing (outputStrLn q >> getInputLine prompt)
