{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}

module LibraryDB
    ( exec
    ) where

import Data.Aeson
import Options.Applicative
import Data.Time.Clock
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Data.Aeson.Encode.Pretty (encodePretty)
import Text.Email.Validate
import Control.Lens.Tuple
import Control.Lens.Iso
import Control.Lens.At
import Data.Char (toLower)
import Data.List (sortBy)
import Control.Monad (liftM2, mapM)
import Types
import QRCodes

exec :: IO ()
exec = do
    createBook $ newBook "In pursuit of pure reason" "Vanessa McHale"
    (checkout (createPatron "The" "a@a.com") (newBook "In pursuit of pure reason" "Vanessa McHale")) >>= createQRCode
    (fmap head) getBookDB >>= showObject

--ideally the qrcodes should be cryptographically signed?

showObject :: (ToJSON a) => a -> IO ()
showObject = BSL.putStrLn . encodePretty

newBook :: String -> String -> Book
newBook tit aut = Book { _title           = tit
                          , _author          = aut
                          , _isbn            = Nothing
                          , _publisher       = Nothing
                          , _publicationYear = Nothing
                          , _checkoutLength  = 7 } --7 days by default

updateRecord :: (ToJSON a) => a -> FilePath -> IO ()
updateRecord rec file = BSL.appendFile file toAppend where
    toAppend = (encode rec) `BSL.append` (BSL.singleton '\n')

createBook :: Book -> IO ()
createBook = flip updateRecord "db/library.json"

daysToDiffTime :: Integer -> DiffTime
daysToDiffTime = secondsToDiffTime . (*86400)

createPatron :: String -> String -> Patron
createPatron nam ema = Patron { _name   = nam
                            , _record = []
                            , _email  = email }
    where email = BS.unpack $ (toByteString . emailError) $ (emailAddress . BSL.toStrict . BSL.pack) ema

newPatron :: Patron -> IO ()
newPatron = flip updateRecord "db/patron.json"

emailError :: Maybe EmailAddress -> EmailAddress
emailError (Just a) = a
emailError Nothing = error "Email format not valid" --actually maybe just repeat or something? in optparse-applicative or gen. a dsl idk

isDue :: Book -> IO Bool
isDue = (\a -> return False)
--search the db idk

sendReminders :: IO ()
sendReminders = do
    --search users for late books and then send them emails
    putStrLn "...emails sent successfully"

findByPatron :: Patron -> [Book]
findByPatron pat = map (view _1) $ view (record . simple) pat

checkout :: Patron -> Book -> IO Patron
checkout pat boo = getCurrentTime >>= (\time -> return $ over (record) ((:) (boo, time)) pat)

searchByAuthor :: String -> IO [Book]
searchByAuthor aut = fmap (filter (\boo -> view (author) boo == aut)) getBookDB

searchByTitle :: String -> IO [Book]
searchByTitle tit = fmap (filter (\boo -> (==) (map toLower $ view (title) boo) (map toLower tit))) getBookDB
--use toLower to make it case insensitive

--sortByDueDate :: [Book] -> IO [Book]
--sortByDueDate boo = boo >>= ((\boo1 boo2 -> (liftM2 compare) (dueDate boo1) (dueDate boo2)) >>= sortBy)
--boo >>= (sortBy (\boo1 boo2 -> (liftM2 compare) <$> dueDate <*> boo1 <*> boo2))

dueDate :: Book -> IO UTCTime
dueDate boo = fmap ((view _2 . head) . (filter (\i -> (==) boo (view (_1) i)))) bookPairs --addUTCTime does something stupid idk

bookPairs :: IO [(Book, UTCTime)]
bookPairs = fmap (concat . (map (view record))) getPatrons

getRecord :: (FromJSON a) => FilePath -> IO [a]
getRecord path = fmap (map (stripJSON . decode')) (fmap BSL.lines (BSL.readFile path))

getBookDB :: IO [Book]
getBookDB = getRecord "db/library.json"

stripJSON :: Maybe a -> a
stripJSON (Just a) = a
stripJSON Nothing  = error "Failed to parse database."

getPatrons :: IO [Patron]
getPatrons = getRecord "db/patron.json"
