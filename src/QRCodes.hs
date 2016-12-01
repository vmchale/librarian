{-# LANGUAGE GADTs #-}

module QRCodes where

import Data.Aeson
import Data.QRCode
import Codec.Picture.Types
import Codec.Picture.Png (writePng)
import Data.Word (Word8)
import Data.Vector.Storable as V
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString, pack, unpack)
import Data.List (replicate)
import Data.Char (toLower)
import Prelude as P
import Crypto.PubKey.RSA as Cr
import Jose.Jws
import System.Directory (doesFileExist)
import Control.Lens.Tuple
import Control.Lens (view)
import Jose.Jwt (unJwt)
import Jose.Jwa (JwsAlg (RS256))
import Data.Either (either)
import Jose.Jwt (JwtError)
import Jose.Jws (rsaDecode)
import Data.Bits ((.&.))

checkSig :: ByteString -> IO (Either JwtError ByteString)
checkSig tok = do
    key <- fmap read $ readFile "key.hk"
    let jws = rsaDecode key tok
    return $ (fmap (view _2)) jws

createSecureQRCode :: (ToJSON a) => a -> FilePath -> IO ()
createSecureQRCode object filepath = regenerate filepath make
    where make = do
                    switch <- doesFileExist "key.hk"
                    if not switch then do
                        putStrLn "generating key..."
                        key <- Cr.generate 256 0x10001
                        writeFile "key.hk" (show key)
                    else
                        return ()
                    key' <- fmap read $ readFile "key.hk" :: IO (Cr.PublicKey, Cr.PrivateKey)
                    signedToken <- rsaEncode RS256 (view _2 key') (toStrict $ encode object)
                    let signed = fmap (unJwt) signedToken
                    output <- liftEither id $ fmap (flip byteStringToQR filepath) signed
                    putStrLn $ show output

regenerate :: FilePath -> IO () -> IO ()
regenerate filepath action = do { regen <- doesFileExist filepath ; if regen then putStrLn "already generated, skipping..." else action }

liftEither :: (Show b, Monad m) => (t -> m a) -> Either b t -> m a
liftEither = either (fail . show)

createQRCode :: (ToJSON a) => a -> FilePath -> IO ()
createQRCode object filepath = regenerate filepath make
    where make = let input = toStrict $ encode object in byteStringToQR input filepath

byteStringToQR :: ByteString -> FilePath -> IO ()
byteStringToQR input filepath = do
    smallMatrix <- (fmap toMatrix) $ encodeByteString input Nothing QR_ECLEVEL_H QR_MODE_EIGHT False
    let qrMatrix = fattenList 8 $ P.map (fattenList 8) smallMatrix
    writePng filepath (encodePng qrMatrix)

--encodePng' :: QRCode -> Image Word8
--encodePng' code = Image dim dim vector
--    where dim     = getQRCodeWidth code
--          vector  = V.map tobin . unpack' $ (getQRCodeString code)
--          tobin c = c .&. 1

--unpack' :: ByteString -> Vector (Word8)
--unpack' bs = Vector { 

encodePng :: [[Word8]] -> Image Word8
encodePng matrix = Image dim dim vector
    where dim    = P.length matrix
          vector = V.map ((*255) . swapWord) $ fromList $ P.concat matrix

fattenList :: Int -> [a] -> [a]
fattenList i l = P.concat $ (P.foldr ((:) . (P.replicate i)) [] l)

swapWord :: Word8 -> Word8
swapWord 1 = 0
swapWord 0 = 1
