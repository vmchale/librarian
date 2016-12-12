{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}

module Internal.Types where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Control.Lens (makeLenses)
import Data.Aeson (ToJSON, FromJSON)

data Book = Book { _title           :: String
                 , _author          :: String
                 , _isbn            :: Maybe String
                 , _publisher       :: Maybe String
                 , _publicationYear :: Maybe String
                 , _checkoutLength  :: Integer --in days
                 } deriving (Show, Generic, Eq)

makeLenses ''Book

instance ToJSON Book where

instance FromJSON Book

data Patron = Patron { _name   :: String
                     , _record :: [(Book, UTCTime)]
                     , _email  :: String --add birth date idk?
                     } deriving (Show, Generic, Eq)
makeLenses ''Patron

instance ToJSON Patron where

instance FromJSON Patron
