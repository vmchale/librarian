import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import LibraryDB
import Internal
import Data.Foldable (fold)

main = do 
    fold [ removeDirectoryRecursive "db"
         , removeDirectoryRecursive "hamlet/cards"
         , createDirectoryIfMissing True "db/bib"
         , createDirectoryIfMissing True "hamlet/cards"
         ]
    exec
