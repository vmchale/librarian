import QRCodes
import Data.ByteString as B
import System.Environment (getArgs)

main :: IO ()
main = do
    pipeIn <- B.getContents
    filepath <- fmap (flip (!!) 0) getArgs
    byteStringToQR pipeIn filepath
