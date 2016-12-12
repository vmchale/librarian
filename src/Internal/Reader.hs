module Internal.Reader where

import System.Process

install :: IO [IO ()]
install = map putStr <$> readCommands "src/Internal/completions-cmd"

readCommands :: FilePath -> IO [String]
readCommands filepath = do
    file <- readFile filepath
    mapM ((flip readCreateProcess "") . shell) $ lines file
