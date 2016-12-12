{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module exports some functions to generate QR codes from objects that are part of the ToJSON class
-- | as well as functions that work on byteStrings.
module Generators.QRCodes (createSecureQRCode
                          , createQRCode
                          , byteStringToQR
                          , poopJSON
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
import Jose.Jwt (unJwt, JwtError)
import Jose.Jwa (JwsAlg (RS512))
import Data.Either (either)
import Data.Bits ((.&.))
import Control.Applicative ((<$>))

-- | check signature on token
checkSig tok = do
    key <- read <$> readFile ".key.hk"
    let jws = rsaDecode key tok
    return $ fmap (view _2) jws

-- | create signed QR code from an object that is a member of the ToJSON class
createSecureQRCode :: (ToJSON a) => a -> FilePath -> IO ()
createSecureQRCode object filepath = regenerate filepath make
    where make = do
                    switch <- doesFileExist ".key.hk"
                    if not switch then do
                        putStrLn "generating key..."
                        key <- Cr.generate 512 0x10001
                        writeFile ".key.hk" (show key)
                    else
                        return ()
                    key' <- read <$> readFile ".key.hk" :: IO (Cr.PublicKey, Cr.PrivateKey)
                    signedToken <- rsaEncode RS512 (view _2 key') (toStrict $ encode object)
                    let signed = fmap unJwt signedToken
                    output <- liftEither id $ fmap (flip byteStringToQR filepath) signed
                    print output

-- | only write to filepath if the file does not currently exist
regenerate :: FilePath -> IO () -> IO ()
regenerate filepath action = do { regen <- doesFileExist filepath ; if regen then putStrLn "already generated, skipping..." else action }

-- | lift an Either IO to an IO
liftEither :: (Show b, Monad m) => (t -> m a) -> Either b t -> m a
liftEither = either (fail . show)

-- | put an object that is a member of the ToJSON class in a file
poopJSON :: (ToJSON a) => a -> FilePath -> IO ()
poopJSON object filepath = BSL.writeFile filepath (encode object)

-- | Create a QR Code from an object that is a member of the ToJSON class
createQRCode :: (ToJSON a) => a -> FilePath -> IO ()
createQRCode object filepath = regenerate filepath make
    where make = let input = toStrict $ encode object in byteStringToQR input filepath

-- | Create a QR code from a ByteString
byteStringToQR :: BS.ByteString -> FilePath -> IO ()
byteStringToQR input filepath = do
    smallMatrix <- toMatrix <$> encodeByteString input Nothing QR_ECLEVEL_H QR_MODE_EIGHT False
    let qrMatrix = fattenList 8 $ P.map (fattenList 8) smallMatrix
    writePng filepath (encodePng qrMatrix)

encodePng :: [[Word8]] -> T.Image Word8
encodePng matrix = Image dim dim vector
    where dim    = P.length matrix
          vector = V.map ((*255) . swapWord) $ V.fromList $ P.concat matrix

fattenList :: Int -> [a] -> [a]
fattenList i l = P.concat $ P.foldr ((:) . (P.replicate i)) [] l

swapWord :: Word8 -> Word8
swapWord 1 = 0
swapWord 0 = 1
