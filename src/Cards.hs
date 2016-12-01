{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Cards (exec
             ) where

import Text.Hamlet
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (Html)
import System.IO (writeFile)
import System.Process
import Data.Foldable (fold)
import Types
import Control.Lens
import LibraryDB
import Data.List.Utils (replace)

render = undefined

exec :: IO ()
exec = fold [ make ]--, cardTest ]

defHtml nam ema =
    $(hamletFile "hamlet/default.hamlet")

make :: IO ()
make = do
    inputFiles <- fmap (map (\p -> defHtml (view name p) (view email p))) getPatrons
    outputFiles <- fmap (map ((flip (++) ".html") . (takeWhile (/= '@')) . (view email))) getPatrons
    users <- getPatrons
    sequence_ $ zipWith3 (\i o u -> writeFile ((++) "hamlet/cards/" o) $ (rightImage (view email u)) $ renderHtml $ i render) inputFiles outputFiles users

rightImage :: String -> String -> String
rightImage ema = replace "FILENAME_HERE" ("../../db/cards/" ++ ema ++ ".png")

cardTest :: IO ()
cardTest = readCreateProcess (shell "pandoc hamlet/cards/tmchale.html -f html -t LaTeX -o hamlet/cards/card.pdf && gnome-open hamlet/cards/card.pdf") "" >>= print
