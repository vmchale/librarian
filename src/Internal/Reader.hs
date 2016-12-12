-- | Module containing functions to add command-line completions
module Internal.Reader where

import System.Process

-- | Install command-line completions
install :: IO [IO ()]
install = map putStr <$> readCommands "src/Internal/completions-cmd"

-- | Read shell commands from a file
readCommands :: FilePath -> IO [String]
readCommands filepath = do
    file <- readFile filepath
    mapM ((flip readCreateProcess "") . shell) $ lines file
