module FQuoter.Display 
( displayQuotes )
where

import System.Console.Haskeline
import Data.Maybe
import Data.List 

import FQuoter.Quote
import FQuoter.Config.Config
import FQuoter.Templating.TemplateParser
import FQuoter.Templating.Display
import FQuoter.Serialize.SerializedTypes

displayQuotes :: Config -> [SerializedType] -> InputT IO ()
displayQuotes _ [] = outputStrLn "No result !"
displayQuotes conf st 
    = case parser of
            Left err -> do let msg = ["Configuration file faulty."
                                     ,show err
                                     ,"Template cannot be parsed."
                                     ,"Falling back on default config."]
                           mapM_ outputStrLn msg
                           displayQuotes buildDefaultConfig st
            Right x -> mapM_ (displayQuote x) st
    where
        parser = parseTemplate (currentTemplate conf)

displayQuote template (SQuote q) = mapM_ outputStrLn $ catMaybes displayed
    where
        displayed = [Just line
                    , Just $ "\"" ++ content q ++ "\""
                    , Just ""
                    , Just $ readTree template q
                    , comment q 
                    , Just ""
                    , outputTagsArray $ tags q
                    , Just line]
        outputTagsArray [] = Nothing
        outputTagsArray xs = Just $ "Tags : " ++ intercalate "," xs
        line = replicate 80 '-' 
displayQuote _ _ = error "Not a quote. Should not happen."
