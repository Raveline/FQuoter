module FQuoter.Parser.ParsingErrors
where

errorNoParsing :: String
errorNoParsing = "Unkown command. Available commands : insert, find, search, update, delete."
errorInsertNoType :: String
errorInsertNoType = "Insert what ? Available insertions : author, source, quote."
errorSearchNoParam :: String
errorSearchNoParam = "Search what ? Enter a word or tags inside brackets (e.g. [english,poetry])"
