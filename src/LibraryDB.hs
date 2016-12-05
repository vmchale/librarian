{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}

module LibraryDB where

import Data.Aeson
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
import Control.Monad (mapM, join)
import Control.Applicative (liftA2, (<$>))
import Types
import QRCodes
import Data.Foldable (fold)
import System.Directory
import Control.Monad.ListM
import Network.Mail.SMTP
import Network.Mail.Mime hiding (simpleMail)
import qualified Data.Text as T 

mkLabel :: Book -> IO ()
mkLabel boo = fold [ createQRCode boo (name ++ ".png")
                   , poopJSON boo (name ++ ".json")
                   ]
    where name = filter (not . ((flip elem) (":;&#" :: String))) $ "db/labels/" ++ (map (toLower . (\c -> if c==' ' then '-' else c)) (take 60 (view title boo)))

mkCard :: Patron -> IO ()
mkCard pat = do 
    createQRCode pat ("db/cards/" ++ pat^.email ++ ".png")
    poopJSON pat ("db/cards/" ++ pat^.email ++ ".json")
    createSecureQRCode pat ("db/cards/" ++ pat^.email ++ "-signed.png")

updatePatron :: Patron -> IO ()
updatePatron p = fold [ deletePatron p
                     ,  newPatron p ]

updateField :: a -> Lens' a b -> b -> a
updateField obj lens new = set lens new obj

updateQR :: IO ()
updateQR = do
    p <- getPatrons
    b <- getBookDB
    sequence_ $ map mkLabel b
    sequence_ $ map mkCard p

showObject :: (ToJSON a) => a -> IO ()
showObject = BSL.putStrLn . encodePretty

newBook :: String -> String -> Book
newBook tit aut = Book { _title           = tit
                       , _author          = aut
                       , _isbn            = Nothing
                       , _publisher       = Nothing
                       , _publicationYear = Nothing
                       , _checkoutLength  = 21 } --21 days by default

updateRecord :: (ToJSON a) => a -> FilePath -> IO ()
updateRecord rec file = BSL.appendFile file toAppend where
    toAppend = (encode rec) `BSL.append` (BSL.singleton '\n')

updateByCard :: Patron -> Book -> IO Patron
updateByCard pat boo = (matchRecord pat) >>= ((flip checkout) boo)

matchRecord :: Patron -> IO Patron
matchRecord pat = fmap (head . (filter (\a -> pat^.email == a^.email))) getPatrons

createBook :: Book -> IO ()
createBook = flip updateRecord "db/library.json"

replaceRecord :: (ToJSON a) => [a] -> FilePath -> IO ()
replaceRecord list file = do
    removeFile file
    fold $ map (flip updateRecord file) list

deleteBook :: Book -> IO ()
deleteBook boo = do
    newDB <- filter (/=boo) <$> getBookDB
    replaceRecord newDB "db/library.json"

deletePatron :: Patron -> IO ()
deletePatron pat = do
    p <- matchRecord pat
    newDB <- fmap (filter (/=p)) getPatrons
    replaceRecord newDB "db/patron.json"

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
emailError Nothing = error "Email format not valid"

isDue :: Book -> IO Bool
isDue = ((liftA2 (>=)) getCurrentTime) . dueDate

userDelinquencies :: Patron -> IO [Book]
userDelinquencies = (=<<) (filterMP isDue) . return . findByPatron

delinquentUsers :: IO [Patron]
delinquentUsers = allDueBooks >>= sequence . (map patronByBook)

allDueBooks :: IO [Book]
allDueBooks = getBookDB >>= (filterMP isDue)

sendReminders :: IO ()
sendReminders = do
    delinquentUsers >>= print --for debug only
    map (sender . emailGen) <$> delinquentUsers
    putStrLn "...emails sent successfully"

sender :: Mail -> IO ()
sender mail = (readFile "pw") >>= (\pw -> sendMailWithLogin' "smtp.gmail.com" 465 "vamchale@gmail.com" pw mail)

emailGen :: Patron -> Mail
emailGen pat = simpleMail (Address Nothing "library@vmchale.com") ([Address (Just (T.pack $ view name pat)) (T.pack $ view email pat)]) [] [] "Overdue Books" [(plainTextPart msg)]
    where msg = "Our records indicate you have an overdue book." --make it actually useful

findByPatron :: Patron -> [Book]
findByPatron pat = map (view _1) $ view (record . simple) pat

checkout :: Patron -> Book -> IO Patron
checkout pat boo = getCurrentTime >>= (\time -> return $ over (record) ((:) (boo, time)) pat)

patronByBook :: Book -> IO Patron
patronByBook boo = head . (filter (\p -> boo `elem` (map (view _1) (p^.record)))) <$> getPatrons

renew :: Book -> IO Patron
renew boo = do
    pat <- patronByBook boo
    let p' = return' pat boo
    checkout p' boo

return' :: Patron -> Book -> Patron
return' pat boo = over (record) (filter ((/= boo) . (view _1))) pat

searchByAuthor :: String -> IO [Book]
searchByAuthor aut = fmap (filter (\boo -> view (author) boo == aut)) getBookDB

searchByTitle :: String -> IO [Book]
searchByTitle tit = fmap (filter (\boo -> (==) (map toLower $ view (title) boo) (map toLower tit))) getBookDB

sortByDueDate :: [Book] -> IO [Book]
sortByDueDate = sortByM (\boo1 boo2 -> (liftA2 compare) (dueDate boo1) (dueDate boo2))

dueDate :: Book -> IO UTCTime
dueDate boo = fmap ((addUTCTime t) . (view _2 . head) . (filter (\i -> (==) boo (view (_1) i)))) bookPairs
    where t = fromInteger ((*604800) $ (view checkoutLength boo))

bookPairs :: IO [(Book, UTCTime)]
bookPairs = fmap (concat . (map (view record))) getPatrons

getRecord :: (FromJSON a) => FilePath -> IO [a]
getRecord path = do
    tmp <- getTemporaryDirectory
    let path'= (tmp ++ "/" ++ (reverse $ takeWhile (/= '/') $ reverse path))
    copyFile path path'
    fmap ((map (stripJSON . decode')) . BSL.lines) (BSL.readFile path')

getBookDB :: IO [Book]
getBookDB = getRecord "db/library.json"

stripJSON :: Maybe a -> a
stripJSON (Just a) = a
stripJSON Nothing  = error "Failed to parse database."

getPatrons :: IO [Patron]
getPatrons = getRecord "db/patron.json"
