module Search.Finders ( searchByAuthor
                      , searchByTitle ) where

import Internal.Types
import LibraryDB
import Control.Lens (view)
import Data.Char (toLower)

-- | Return a book from the DB, by author
searchByAuthor :: String -> IO [Book]
searchByAuthor aut = fmap (filter (\boo -> (squash $ view (author) boo) == (squash aut))) getBookDB

-- | Return a book from the DB, by title string (ignoring case)
searchByTitle :: String -> IO [Book]
searchByTitle tit = fmap (filter (\boo -> (==) (squash $ view (title) boo) (squash tit))) getBookDB
    
squash = (filter (not . (`elem` " ,;.'"))) . (map toLower)
