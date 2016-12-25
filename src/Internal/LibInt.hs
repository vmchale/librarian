{-# LANGUAGE RankNTypes #-}
-- | Low-level functions LibraryDB uses
module Internal.LibInt where

import Data.Time.Clock
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Text.Email.Validate
import Network.Mail.Mime
import Network.Mail.SMTP
import System.Directory
import Internal.Types
import Data.Foldable (fold)
import System.Process
import Data.String.Utils (replace)
import Data.Char (toLower)
import Data.Composition ((.*))
import Control.Concatenative (bi)
import Data.Function (on)
import Control.Lens
import System.IO.Unsafe (unsafePerformIO)

data RecordType = Png | Json deriving Eq

-- | convert an integer number of days to DiffTime for use with Data.Time etc.
daysToDiffTime :: Integer -> DiffTime
daysToDiffTime = secondsToDiffTime . (*86400)

-- | Pretty-print an object with a ToJSON instance
showObject :: (ToJSON a) => a -> IO ()
showObject = BSL.putStrLn . encodePretty

stripJSON :: Maybe a -> a
stripJSON (Just a) = a
stripJSON Nothing  = error "Failed to parse database."

-- | Convert a Maybe EmailAddress to an EmailAddress
emailError :: Maybe EmailAddress -> EmailAddress
emailError (Just a) = a
emailError Nothing = error "Email format not valid"

-- | Get a list of objects of the FromJSON class from a file with one object per line
getRecord :: (FromJSON a) => FilePath -> IO [a]
getRecord path = do
    tmp <- getTemporaryDirectory
    let path'= (tmp ++ "/" ++ (reverse $ takeWhile (/= '/') $ reverse path))
    copyFile path path'
    fmap ((map (stripJSON . decode')) . BSL.lines) (BSL.readFile path')

-- | Append a record to the appropriate file
updateRecord :: (ToJSON a) => a -> FilePath -> IO ()
updateRecord rec file = BSL.appendFile file toAppend where
    toAppend = (encode rec) `BSL.append` (BSL.singleton '\n')

-- | Replace a record with a list of JSON objects.
replaceRecord :: (ToJSON a) => [a] -> FilePath -> IO ()
replaceRecord list file = do
    removeFile file
    fold $ map (flip updateRecord file) list

-- | Send mail from the smtp server indicated in the file `.pw`
sender :: Mail -> IO ()
sender mail = do
    pwFile <- readFile ".pw"
    let [domain, port, uname, pw] = map filtrant $ lines pwFile where filtrant = reverse . (takeWhile (not . (`elem` ": "))) . reverse
    sendMailWithLogin' domain (read port) uname pw mail

fixStr :: [(String, String)] -> String -> String
fixStr = (foldr (.) id) . (map (uncurry replace))

-- | Hamming distance on a list
hammingDistance :: (Eq a, Integral b) => [a] -> [a] -> b
hammingDistance = sum .* (zipWith (\i j -> if i == j then 1 else 0))

-- | Make everything lowercase, and filter out possible junk characters
squash :: String -> String
squash = (filter (not . (`elem` " ,;.'"))) . (map toLower)

--look up sig of biSp if stuck again
-- | map over a pair of lists with a pair of functions, then zip the lists together
zipMap :: (a -> b) -> (a -> c) -> [a] -> [(b, c)]
zipMap f g = bi (fmap f) (fmap g) zip 

-- | Name a file given a book
nameBook :: RecordType -> Book -> FilePath
nameBook ext = nameFile "db/labels/" ext title

-- | Name a file, given a base directory and lens from which to name a record.
nameFile :: FilePath -> RecordType -> Lens' a String -> a -> FilePath
nameFile dir ext lens obj = filter (not . ((flip elem) (":;&#" :: String))) $ dir ++ (map (toLower . (\c -> if c==' ' then '-' else c)) (take 60 (view lens obj))) ++ (if ext == Png then ".png" else ".json")

badCat :: [IO [a]] -> IO [a]
badCat = (pure) . (concat) . (map unsafePerformIO)
