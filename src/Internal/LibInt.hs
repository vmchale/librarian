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
