-- | Module with functions to parse .ris files
module Parser ( parseBib
              ) where

import System.Directory
import Internal.Types
import LibraryDB
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Control.Applicative (liftA2)

-- | parse a directory of .ris files and add them to the json-poopDB
parseBib :: FilePath -> IO ()
parseBib dir = do
    contents <- filter ((=="sir.") . (take 4) . reverse) <$> listDirectory dir
    putStrLn "parsing bib files..."
    sequence_ $ map (((flip (>>=)) mkLabel) . mkBook . ((++) "db/bib/")) contents

-- | make a book from a .ris file
-- | This should use actual parsers in the future so it doesn't read the file 4 times for no reason
mkBook :: FilePath -> IO Book
mkBook filepath = do
    boo <- ((liftA2 newBook) (title' filepath) (author' filepath))
    p <- publicationYear' filepath
    let b' = boo { _publicationYear = pure p }
    createBook b'
    return b'

-- | get the author from a .ris file
author' :: FilePath -> IO String
author' filepath = do
    ls <- lines <$> readFile filepath
    return $ takeWhile (not . ((flip elem) "(")) $ (drop 6) $ (head $ filter (\l -> "AU  - " `isPrefixOf` l) ls)

-- | get the title from a .ris file
title' :: FilePath -> IO String
title' filepath = do
    ls <- lines <$> readFile filepath
    return $ (drop 6) $ (head $ filter (\l -> "T1  - " `isPrefixOf` l) ls)

publicationYear' :: FilePath -> IO String
publicationYear' filepath = do
    ls <- lines <$> readFile filepath
    return $ (take 4) . (filter isDigit) $ (drop 6) $ (head $ filter (\l -> "PY  - " `isPrefixOf` l) ls)
