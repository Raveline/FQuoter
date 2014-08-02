module FQuoter.Parser.ParsingErrors
where

errorNoParsing = "Unkown command. Available commands : insert, find, search, update, delete."
errorInsertNoType = "Insert what ? Available insertions : author, source, quote."
errorSearchNoParam = "Search what ? Enter a word or tags inside brackets (e.g. [english,poetry])"
