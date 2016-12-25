{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module containing the basic building blocks and functions to manage a home library.
-- | You can also manage the library using the provided `library` executable.
module LibraryDB ( mkLabel
                 , mkCard
                 , updatePatron
                 , updateField
                 , updateQR
                 , newBook
                 , matchRecord
                 , createBook
                 , deleteBook
                 , deletePatron
                 , createPatron
                 , newPatron
                 , userDelinquencies
                 , delinquentUsers
                 , sendReminders
                 , findByPatron
                 , checkout
                 , patronByBook
                 , renew
                 , return'
                 , sortByDueDate
                 , dueDate
                 , getBookDB
                 , getPatrons
                 , getQRBook
                 , module Internal.Types
                 ) where

import Data.Aeson
import Data.Time.Clock
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Lens.Tuple
import Control.Lens.Iso
import Control.Lens.At
import Text.Email.Validate
import Data.Char (toLower)
import Data.List (sortBy)
import Control.Monad (mapM, join)
import Control.Applicative (liftA2, (<$>))
import Internal.Types
import Generators.QRCodes
import Data.Foldable (fold)
import System.Directory
import Control.Monad.ListM
import Network.Mail.SMTP
import Network.Mail.Mime hiding (simpleMail)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Internal.LibInt

-- | make a JSON record and QR code label for a given book
mkLabel :: Book -> IO ()
mkLabel boo = fold [ regenQRCode boo (nameBook Png boo)
                   , poopJSON boo (nameBook Json boo)
                   ]
                   
-- | make a JSON record, QR code, and signed QR Code for a given user
mkCard :: Patron -> IO ()
mkCard pat = do 
    regenQRCode pat ("db/cards/" ++ pat^.email ++ ".png")
    poopJSON pat ("db/cards/" ++ pat^.email ++ ".json")
    --regenSecureQRCode pat ("db/cards/" ++ pat^.email ++ "-signed.png")

-- | update a patron in the database
updatePatron :: Patron -> IO ()
updatePatron p = (fold . (map ($ p))) [ deletePatron
                                      , newPatron ]

-- | update a field of an object, returning another object
updateField :: a -> Lens' a b -> b -> a
updateField obj lens new = set lens new obj

-- | Apply mkCard to all patrons
updateQR :: IO ()
updateQR = do
    p <- getPatrons
    mapM_ mkCard p

-- | Create a new book from a string containing the title and a string containing the author
newBook :: String -> String -> Book
newBook tit aut = Book { _title           = tit
                       , _author          = aut
                       , _isbn            = Nothing
                       , _publisher       = Nothing
                       , _publicationYear = Nothing
                       , _checkoutLength  = 21 } --21 days by default


-- | Given a patron's basic info and a book, return a new patron
updateByCard :: Patron -> Book -> IO Patron
updateByCard pat boo = (matchRecord pat) >>= ((flip checkout) boo)

-- | Match a patron's basic info to full record
matchRecord :: Patron -> IO Patron
matchRecord pat = fmap (head . (filter (\a -> pat^.email == a^.email))) getPatrons

-- | Given a book, append it to the db.
createBook :: Book -> IO ()
createBook = flip updateRecord "db/library.json"

-- | remove a book from the DB
deleteBook :: Book -> IO ()
deleteBook boo = do
    newDB <- filter (/=boo) <$> getBookDB
    replaceRecord newDB "db/library.json"

-- | remove a patron from the DB
deletePatron :: Patron -> IO ()
deletePatron pat = do
    p <- matchRecord pat
    newDB <- fmap (filter (/=p)) getPatrons
    replaceRecord newDB "db/patron.json"

-- | Create a patron with some defaults
createPatron :: String -> String -> Patron
createPatron nam ema = Patron { _name   = nam
                              , _record = []
                              , _email  = email }
    where email = BS.unpack $ (toByteString . emailError) $ (emailAddress . BSL.toStrict . BSL.pack) ema

-- | Add a patron object to the db
newPatron :: Patron -> IO ()
newPatron = flip updateRecord "db/patron.json"

isDue :: Book -> IO Bool
isDue = ((liftA2 (>=)) getCurrentTime) . dueDate

-- | Return a list of overdue books for a given user
userDelinquencies :: Patron -> IO [Book]
userDelinquencies = (=<<) (filterMP isDue) . return . findByPatron

-- | Return a list of delinquent users
delinquentUsers :: IO [Patron]
delinquentUsers = allDueBooks >>= sequence . (map patronByBook)

-- | Return a list of books which are due
allDueBooks :: IO [Book]
allDueBooks = getBookDB >>= (filterMP isDue)

-- | Send reminder emails to all patrons
sendReminders :: IO ()
sendReminders = do
    del <- delinquentUsers
    sequence_ $ map ((fmap sender) . (emailGen)) del
    putStrLn "...emails sent successfully"

-- | Scold a patron for being late - use only in conjuction with `sendReminders`
emailGen :: Patron -> IO Mail
emailGen pat = do
    del <- userDelinquencies pat
    let msg = "Our records indicate you have overdue books, viz.\n" `TL.append` (fold $ map (TL.pack . show . (view title)) del)
    return $ simpleMail (Address Nothing "library@vmchale.com") ([Address (Just (T.pack $ view name pat)) (T.pack $ view email pat)]) [] [] "Overdue Books" [(plainTextPart msg)]

-- | Given a patron, find all books they have out
findByPatron :: Patron -> [Book]
findByPatron pat = map (view _1) $ view (record . simple) pat

-- | Given a patron and book, add the book to their record and return patron object
checkout :: Patron -> Book -> IO Patron
checkout pat boo = getCurrentTime >>= (\time -> return $ over (record) ((:) (boo, time)) pat)

-- | Find a patron that has a particular book out
patronByBook :: Book -> IO Patron
patronByBook boo = head . (filter (\p -> boo `elem` (map (view _1) (p^.record)))) <$> getPatrons

-- | given a book in all lowercase, return the correct book
getBookRecord :: Book -> IO Book
getBookRecord boo = head . (filter (\b -> map toLower (view title boo) == map toLower (view title b))) <$> getBookDB

-- | Given a book, allow the patron who currently has it out 21 days from now
renew :: Book -> IO Patron
renew boo = do
    pat <- patronByBook boo
    let p' = return' pat boo
    checkout p' boo

-- | Given a parton and a book, remove that book from their record and return patron object
return' :: Patron -> Book -> Patron
return' pat boo = over (record) (filter ((/= boo) . (view _1))) pat

-- | Sort a list of books by due date
sortByDueDate :: [Book] -> IO [Book]
sortByDueDate = sortByM (\boo1 boo2 -> (liftA2 compare) (dueDate boo1) (dueDate boo2))

-- | Given a book, returns its due date (after looking it up in the DB)
dueDate :: Book -> IO UTCTime
dueDate boo = fmap ((addUTCTime t) . (view _2 . head) . (filter (\i -> (==) boo (view (_1) i)))) bookPairs
    where t = fromInteger ((*604800) (view checkoutLength boo))

-- | returns a list of books from the DB
getBookDB :: IO [Book]
getBookDB = getRecord "db/library.json"

-- | returns a list of patrons from the DB
getPatrons :: IO [Patron]
getPatrons = getRecord "db/patron.json"

bookPairs :: IO [(Book, UTCTime)]
bookPairs = fmap (concat . (map (view record))) getPatrons

getQRBook :: FilePath -> IO Book
getQRBook boof = do
    boo' <- (stripJSON <$> (readQRCodeObj boof) :: IO Book)
    getBookRecord boo'
