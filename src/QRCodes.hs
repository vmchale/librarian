{-# LANGUAGE GADTs #-}

module QRCodes where

import Data.Aeson
import Data.QRCode
import Data.Word (Word8)
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
import Data.Vector.Storable.ByteString (byteStringToVector)
import qualified Codec.Picture.Repa as CR
import Data.Array.Repa as R
import Control.Lens (view)
import Control.Lens.Tuple
import Data.Array.Repa.IO.DevIL
import qualified Codec.Picture.Repa as CR
--import Data.Vector as V
import Data.Array.Repa.Repr.Vector
import Data.Vector.Storable as V

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
    qrMatrix <- encodeByteString input Nothing QR_ECLEVEL_H QR_MODE_EIGHT False-- (fmap toMatrix) $ encodeByteString input Nothing QR_ECLEVEL_H QR_MODE_EIGHT False
    --let qrMatrix = smallMatrix -- fattenList 8 $ P.map (fattenList 8) smallMatrix
    --writePng filepath ((scale . encodePng') qrMatrix)
    let toWrite = (encodePng'' qrMatrix)
    liftEither id $ fmap (runIL) $ fmap (writeImage filepath) toWrite

--this is significantly faster so now I just need to scale stuff
--encodePng'' :: QRCode -> IO Img RGB
encodePng'' code = do
    let arr = scale encodePng' code
    --fmap (R.map ((*255) . swapWord . toBin)) $ 
    ((fmap RGB) . copyP . computeVectorP) (extendRows arr)

extendRows :: R.Array DIM3 V Word8 -> R.Array DIM3 V Word8
extendRows = extend (Any :. Any :. (2::Int))

--encodePng' QRCode -> R.Array V DIM2 Word8
--encodePng' code = (Image dim dim vector) :: Image Word8
encodePng' code = fromVector (Z:.dim:dim) vector
    where dim     = getQRCodeWidth code
          vector  = V.map ((*255) . swapWord . toBin) (byteStringToVector $ getQRCodeString code)
          toBin c = c .&. 1

--scale :: Img RBG -> Img RGB
--scale = CR.onImg (manip)

scale :: R.Array D DIM2 Word8 -> R.Array D DIM2 Word8
scale smol = fromFunction sh (\(Z:.y:.x) -> (view _2 smolFunction) (Z:.(y `div` 2):.(x `div` 2)))
    where smolFunction = toFunction smol
          sh           = Z:.((*2) (d smol)):.((*2) (d smol))
          d            = P.head . listOfShape . extent

--not working why isn't eveything in greyscale already?
--stripDynamic :: DynamicImage -> Image Word8
--stripDynamic (ImageY8 a) = a

--unpack' :: ByteString -> Vector (Word8)
--unpack' = byteStringToVector

--encodePng :: [[Word8]] -> Image Word8
--encodePng matrix = Image dim dim vector
--    where dim    = P.length matrix
--          vector = V.map ((*255) . swapWord) $ fromList $ P.concat matrix

fattenList :: Int -> [a] -> [a]
fattenList i l = P.concat $ (P.foldr ((:) . (P.replicate i)) [] l)

swapWord :: Word8 -> Word8
swapWord 1 = 0
swapWord 0 = 1
