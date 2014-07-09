import System.Environment

type Command = String

main :: IO ()
main = do
        args <- getArgs 
        case parseCommand (unwords args) of 
            Left s -> putStrLn s
            Right c -> executeCommand c

parseCommand :: String -> Either String Command
parseCommand s = Left "Nothing implemented yet !"

executeCommand :: Command -> IO ()
executeCommand = undefined
