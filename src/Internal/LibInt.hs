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

daysToDiffTime :: Integer -> DiffTime
daysToDiffTime = secondsToDiffTime . (*86400)

showObject :: (ToJSON a) => a -> IO ()
showObject = BSL.putStrLn . encodePretty

stripJSON :: Maybe a -> a
stripJSON (Just a) = a
stripJSON Nothing  = error "Failed to parse database."

emailError :: Maybe EmailAddress -> EmailAddress
emailError (Just a) = a
emailError Nothing = error "Email format not valid"

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

sender :: Mail -> IO ()
sender mail = do
    pwFile <- readFile ".pw"
    let [domain, port, uname, pw] = map filtrant $ lines pwFile where filtrant = reverse . (takeWhile (not . (`elem` ": "))) . reverse
    sendMailWithLogin' domain (read port) uname pw mail
