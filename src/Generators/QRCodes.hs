{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module exports some functions to generate QR codes from objects that are part of the ToJSON class
-- | as well as functions that work on byteStrings.
module Generators.QRCodes ( regenSecureQRCode
                          , regenQRCode
                          , poopJSON
                          , readQRCodeObj
                          , readQRStrSec
                          ) where

import Data.Aeson
import Data.QRCode
import Codec.Picture.Types as T
import Codec.Picture.Png (writePng)
import Data.Word (Word8)
import qualified Data.Vector.Storable as V
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (replicate)
import Data.Char (toLower)
import Prelude as P
import Crypto.PubKey.RSA as Cr
import Jose.Jws
import System.Directory (doesFileExist)
import Control.Lens.Tuple
import Control.Lens (view)
import Jose.Jwt (unJwt)
import Data.Either (either)
import Data.Bits ((.&.))
import Control.Applicative ((<$>))
import Data.QRCodes
import Data.ByteString.Lazy.Char8 (pack)--remove when added to Data.QRCodes
import System.Process --remove when added to Data.QRCodes
import Data.Char (toUpper)
import Internal.LibInt (fixStr)

-- | check signature on token
--checkSig :: BS.ByteString -> IO (Either JwtError BS.ByteString)
checkSig tok = do
    key <- read <$> readFile ".key.hk" :: IO (Cr.PublicKey, Cr.PrivateKey)
    let jws = rsaDecode (view _1 key) tok
    return $ fmap (view _2) jws

-- | create signed QR code from an object that is a member of the ToJSON class
regenSecureQRCode :: (ToJSON a) => a -> FilePath -> IO ()
regenSecureQRCode object filepath = regenerate filepath (createSecureQRCode object filepath)

-- | only write to filepath if the file does not currently exist
regenerate :: FilePath -> IO () -> IO ()
regenerate filepath action = do { regen <- doesFileExist filepath ; if regen then putStrLn "already generated, skipping..." else action }

-- | put an object that is a member of the ToJSON class in a file
poopJSON :: (ToJSON a) => a -> FilePath -> IO ()
poopJSON object filepath = BSL.writeFile filepath (encode object)

-- | Create a QR Code from an object that is a member of the ToJSON class
regenQRCode :: (ToJSON a) => a -> FilePath -> IO ()
regenQRCode object filepath = regenerate filepath (createQRCode object filepath)

-- | Read a QR code from a given file.
readQRCodeObj :: (FromJSON a, Show a) => FilePath -> IO (Maybe a)
readQRCodeObj filepath = do
    str <- readQRString filepath
    let val = decode . pack . (fixStr [("publicationyear", "publicationYear"), ("checkoutlength", "checkoutLength")])  $ str
    return val
