{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}

-- | Module providing the basic data types: `Book` and `Patron`
-- | It also provides lenses and ToJSON/FromJSON instances for both of them.
module Internal.Types where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Control.Lens (makeLenses)
import Data.Aeson (ToJSON, FromJSON)

-- | The data type for a book
data Book = Book { _title           :: String -- Title of the book, as a string
                 , _author          :: String -- Author, as a utf8-string
                 , _isbn            :: Maybe String -- ISBN, optional
                 , _publisher       :: Maybe String -- Publisher, optional
                 , _publicationYear :: Maybe String -- Publication year, optional
                 , _checkoutLength  :: Integer -- Checkout length for the item, in days
                 } deriving (Show, Generic, Eq)

makeLenses ''Book

instance ToJSON Book where

instance FromJSON Book

-- | The data type for a patron
data Patron = Patron { _name   :: String -- Name, as a utf8 string
                     , _record :: [(Book, UTCTime)] -- Patron record, consisting of pairs of books and the time they were checked out
                     , _email  :: String -- Email, as a string
                     } deriving (Show, Generic, Eq)
makeLenses ''Patron

instance ToJSON Patron where

instance FromJSON Patron
