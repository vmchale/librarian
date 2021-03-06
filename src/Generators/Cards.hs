{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

-- | Module for generating library cards in html
module Generators.Cards (makeCards
                        ) where

import Text.Hamlet
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (Html)
import System.IO (writeFile)
import System.Process
import Data.Foldable (fold)
import Internal.Types
import Control.Lens
import LibraryDB
import Data.List.Utils (replace)
import Internal.LibInt

-- | dummy render function
render = undefined

-- | Generate QR codes for books and library cards for all patrons.
makeCards :: IO ()
makeCards = fold [ updateQR
                 , putStrLn "QR codes generated successfully."
                 , make
                 , printer
                 , libRec ]

-- | default HTML for a given patron's card
defHtml nam ema =
    $(hamletFile "hamlet/default.hamlet")

-- | HTML gathering all the filenames together
allLabels bookList = let bookTitles = map (view title) bookList in
    $(hamletFile "hamlet/printable.hamlet")

-- | Make QR codes & JSON records for every book in the database.
libRec :: IO ()
libRec =(fmap fold) $ getBookDB >>= sequence . (map (mkLabel))

-- | Make library cards (html files) for all users in the database.
make :: IO ()
make = do
    inputFiles <- fmap (map (\p -> defHtml (view name p) (view email p))) getPatrons
    outputFiles <- fmap (map ((flip (++) ".html") . (takeWhile (/= '@')) . (view email))) getPatrons
    users <- getPatrons
    sequence_ $ zipWith3 (\i o u -> writeFile ((++) "hamlet/cards/" o) $ (rightImage (view email u)) $ renderHtml $ i render) inputFiles outputFiles users

printer :: IO ()
printer = do
    books <- getBookDB
    writeFile "hamlet/printed.html" (allBooks books $ renderHtml $ (allLabels books) render)

-- | Link to the correct qr code for the user
rightImage :: String -> String -> String
rightImage ema = replace "FILENAME_HERE" ("../../db/cards/" ++ ema ++ ".png")

bookImage :: Book -> String -> String
bookImage boo = replace ("FILENAME_HERE\">" ++ tit) ("../" ++ (nameBook Png boo) ++ "\">" ) --fix w/ actual title thing
    where tit = view title boo

allBooks :: [Book] -> String -> String
allBooks booList = foldr (.) id [ bookImage boo | boo <- booList ]

-- | Generate a pdf for the user via pandoc (not currently very pretty)
cardTest :: IO ()
cardTest = readCreateProcess (shell "cd hamlet/cards && pandoc tmchale.html -f html -t latex -o card.pdf && gnome-open card.pdf && cd -") "" >>= print
