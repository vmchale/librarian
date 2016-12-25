{-# LANGUAGE RankNTypes #-}

-- | Module providing functions that allow you to search the database
module Search.Finders ( searchByAuthor
                      , searchByTitle
                      -- * Lower level functions
                      , lensBookSearch
                      , lensPatronSearch
                      , fuzzyBookSearch) where

import Internal.Types
import LibraryDB
import Control.Lens
import Data.Function (on)
import Data.List (sortBy)
import Internal.LibInt

-- | Return a book from the DB, by author
searchByAuthor :: String -> IO [Book]
searchByAuthor = lensBookSearch author

-- | Return a book from the DB, by title string (ignoring case)
searchByTitle :: String -> IO [Book]
searchByTitle = lensBookSearch title

-- | Search book DB based on a lens/value
lensBookSearch :: Lens' Book String -> String -> IO [Book]
lensBookSearch lens val = lensSearch lens val squash <$> getBookDB

-- | Search patron DB based on a lens/value
lensPatronSearch :: Lens' Patron String -> String -> IO [Patron]
lensPatronSearch lens val = lensSearch lens val squash <$> getPatrons

-- | Fuzzy search book of DB based on 
fuzzyBookSearch :: Lens' Book String -> String -> IO [Book]
fuzzyBookSearch lens val = (map (^._1)) . (fuzzySearch lens val) <$> getBookDB

-- | "strictness" function to make equivalence have multiple instances-ish
lensSearch :: (Eq c) => Lens' a b -> b -> (b -> c) -> [a] -> [a]
lensSearch lens val equiv = filter (\boo -> (on (==) equiv) (view (lens) boo) (val))

-- | Search 
fuzzySearch :: (Integral c, Eq b) => Lens' a [b] -> [b] -> [a] -> [(a, c)]
fuzzySearch lens val = sort . (mapDistances val)
    where sort           = sortBy (on compare (view _2))
          mapDistances v = zipMap id ((hammingDistance v) . (view lens))
