module Internal where

import System.Process

exec :: IO [IO ()]
exec = map print <$> readCommands "test/cmd-list"

readCommands :: FilePath -> IO [String]
readCommands filepath = do
    file <- readFile filepath
    mapM ((flip readCreateProcess "") . shell) $ lines file
