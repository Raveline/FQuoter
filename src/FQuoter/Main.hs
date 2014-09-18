import Control.Monad.Reader
import System.Environment
import System.Console.Haskeline

import FQuoter.Commands
import FQuoter.Config.Config
import FQuoter.Parser.Parser

main :: IO ()
main = runInputT defaultSettings interpreter

interpreter :: InputT IO ()
interpreter = do args <- liftIO getArgs
                 config <- liftIO readConfig 
                 case parseInput(unwords args) of 
                    Left s -> outputStrLn $ show s
                    Right c -> executeCommand config c

