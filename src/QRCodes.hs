{-# LANGUAGE GADTs #-}

module QRCodes where

import Data.Aeson
import Data.QRCode
import Codec.Picture.Types as T --maybe delete
import Codec.Picture.Png (writePng) -- maybe delete
import Data.Word (Word8)
import qualified Data.Vector.Storable as V --maybe delete
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
import Data.Array.Repa as R
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.Eval (fromList)
import Data.Array.Repa.Repr.ForeignPtr (F)

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
                    output <- liftEither id $ fmap (flip byteStringToQR' filepath) signed
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

byteStringToQR' :: ByteString -> FilePath -> IO ()
byteStringToQR' input filepath = do
    smolMatrix <- (fmap toMatrix) $ encodeByteString input Nothing QR_ECLEVEL_H QR_MODE_EIGHT False
    let qrMatrix = encodePng' smolMatrix
    toWrite <- (flip (>>=)) fatten $ scale qrMatrix
    runIL $ writeImage filepath (RGB toWrite)

--unpack' :: ByteString -> Vector (Word8)
--unpack' bs = byteStringToVector 

scale :: R.Array U DIM2 Word8 -> IO (R.Array F DIM2 Word8)
scale smol = (flip (>>=) computeP) $ return $ fromFunction sh (\(Z:.x:.y) -> ((view _2) (toFunction smol)) (Z:.(x `div` 2):.(y `div` 2)))
    where sh = (\(Z:.x:.y) -> Z:.((*2) x):.((*2) y)) (extent smol)

fatten :: R.Array F DIM2 Word8 -> IO (R.Array F DIM3 Word8) --idk maybe use free monad here?
fatten = computeP . (extend (Any :. All :. All :. (0::Int)))

encodePng' :: [[Word8]] -> R.Array U DIM2 Word8
encodePng' list = fromList sh (concat list)
    where dim = length list
          sh  = (Z:.dim:.dim)

encodePng :: [[Word8]] -> T.Image Word8
encodePng matrix = Image dim dim vector
    where dim    = P.length matrix
          vector = V.map ((*255) . swapWord) $ V.fromList $ P.concat matrix

fattenList :: Int -> [a] -> [a]
fattenList i l = P.concat $ (P.foldr ((:) . (P.replicate i)) [] l)

swapWord :: Word8 -> Word8
swapWord 1 = 0
swapWord 0 = 1
