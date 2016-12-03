module Parser ( exec
              ) where

import System.Directory
import Types
import LibraryDB
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Control.Applicative (liftA2)

exec :: IO ()
exec = do
    contents <- listDirectory "ris-files/"
    sequence_ $ map (((flip (>>=)) mkLabel) . mkBook . ((++) "ris-files/")) contents

--tbh this would be a good place for a monadic parser?? yea. cuz like that's how aeson does it
--although it needs backtracking to work with the publication year/city (centered around the colon)
mkBook :: FilePath -> IO Book
mkBook filepath = do
    boo <- ((liftA2 newBook) (title' filepath) (author' filepath))
    p <- publicationYear' filepath
    return boo { _publicationYear = pure p }

author' :: FilePath -> IO String
author' filepath = do
    ls <- (fmap lines) $ readFile filepath
    return $ takeWhile (not . ((flip elem) "(")) $ (drop 6) $ (head $ filter (\l -> "AU  - " `isPrefixOf` l) ls)

title' :: FilePath -> IO String
title' filepath = do
    ls <- (fmap lines) $ readFile filepath
    return $ (drop 6) $ (head $ filter (\l -> "T1  - " `isPrefixOf` l) ls)

publicationYear' :: FilePath -> IO String
publicationYear' filepath = do
    ls <- (fmap lines) $ readFile filepath
    return $ (take 4) . (filter isDigit) $ (drop 6) $ (head $ (filter (\l -> "PY  - " `isPrefixOf` l)) ls)
