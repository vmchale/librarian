-- | Uses an applicative option parser to pull up functions from LibraryDB
-- | This is what is executed by the executable `library`
module Exec.Options ( exec
                    ) where

import Options.Applicative
import LibraryDB
import Internal.Types
import Internal.LibInt
import Internal.Reader (install)
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson
import Control.Lens (view)
import Generators.Cards (makeCards)
import Parser (parseBib)
import System.Directory
import Data.Foldable (fold)
import Control.Monad (join)

-- | Datatype corresponding to our program
data Program = Program
    { com      :: Com --if left blank, stdin
    , json     :: Bool --if left blank, stdout
    , complete :: Bool --whether to add completions
    , card     :: Maybe FilePath --use json, else .png
    , book'    :: Maybe FilePath }

-- | Datatype corresponding to subcommands
data Com = AddBook { title' :: String , author' :: String }
           | NewPatron { name' :: String, email' :: String }
           | Checkout
           | Return
           | BibImport { dir :: Maybe FilePath }
           | Renew
           | UpdateAll
           | PrintCard { email' :: String }
--with subcommands I could have lenses for my applicative parser combinators? wtheck.

-- | Generates necessary directories and executes program
exec :: IO ()
exec = fold [ fold $ map ((createDirectoryIfMissing True) . ((++) "db/")) ["labels/" , "cards/" , "bib/"]
            , execParser opts >>= pick ]
    where
        opts = info (helper <*> program)
            (fullDesc
            <> progDesc "Manage a library via json-poopDB and make library cards"
            <> header "library - a home library manager"
            )

-- | Converts a 'Program' value to an IO action
pick :: Program -> IO ()
pick (Program i j True k l) = do
    join (fold <$> install)
    putStrLn "bash completions added successfully!"
    pick (Program i j False k l)
pick (Program (PrintCard ema) False _ _ _) = (head . filter (\a -> (==) (view email a) ema)) <$> getPatrons >>= mkCard
pick (Program Checkout True _ (Just patf) (Just boof)) = do
    let pat = (head <$> getRecord patf) >>= matchRecord
    let boo = head <$> getRecord boof
    p' <- checkout <$> pat <*> boo
    p' >>= updatePatron
pick (Program (BibImport (Just dir)) _ _ _ _) = parseBib dir
pick (Program (BibImport Nothing) _ _ _ _) = parseBib "db/bib/"
pick (Program UpdateAll _ _ _ _) = makeCards
pick (Program (AddBook tit aut) False _ _ _) = do 
    let boo = newBook tit aut
    createBook boo
pick (Program (AddBook tit aut) True _ _ _) = do 
    let boo = newBook tit aut
    createBook boo
    showObject boo
pick (Program Return True _ _ (Just boof)) = do
    let boo = head <$> getRecord boof
    let p' = return' <$> (boo >>= patronByBook) <*> boo
    p' >>= updatePatron
pick (Program (NewPatron nam ema) True _ _ _) = do 
    let pat = createPatron nam ema
    newPatron pat
    showObject pat
pick (Program (NewPatron nam ema) False _ _ _) = do 
    let pat = createPatron nam ema
    newPatron pat
pick (Program Renew True _ _ (Just boof)) = do
    boo <- head <$> (getRecord boof :: IO [Book])
    let p' = renew boo
    p' >>= updatePatron

-- | Parser for the 'Program' data type
program :: Parser Program
program = Program
    <$> subparser
            (command "checkout" (info (pure Checkout)
                (progDesc "Check out a book."))
            <> command "return" (info (pure Return)
                (progDesc "Return out a book."))
            <> command "parse-ris" (info 
                (BibImport
                    <$> (optional $ strOption
                        (short 'd'
                        <> long "dir"
                        <> metavar "DIRECTORY"
                        <> help "Directory to search for bib files")))
                (progDesc "Import .ris files to the book databse"))
            <> command "print-card" (info 
                (PrintCard 
                    <$> strOption 
                        (short 'e' 
                        <> metavar "EMAIL" 
                        <> help "email for patron"))
                (progDesc "Print card for a user."))
            <> command "add-user" (info
                (NewPatron
                    <$> strOption
                        (short 'u'
                        <> metavar "NAME"
                        <> help "name for patron")
                    <*> strOption
                        (short 'e'
                        <> metavar "EMAIL"
                        <> help "email of patron"))
                (progDesc "Add user to db."))
            <> command "renew" (info
                (pure Renew)
                (progDesc "Renew a record."))
            <> command "qrgen" (info
                (pure UpdateAll)
                (progDesc "Generate labels for books and patrons."))
            <> command "new-book" (info
                (AddBook
                    <$> strOption
                        (long "title"
                        <> metavar "TITLE"
                        <> help "Title of book")
                    <*> strOption
                        (long "author"
                        <> metavar "AUTHOR"
                        <> help "Author of book"))
                (progDesc "Add book to db."))
            )
    <*> switch
        (long "json"
        <> short 'j'
        <> help "toggles json or QR codes")
    <*> switch
        (long "install"
        <> help "make path completions work in the shell")
    <*> (optional $ strOption
        (long "card"
        <> short 'u'
        <> metavar "CARD"
        <> help "location of library card file"))
    <*> (optional $ strOption
        (long "book"
        <> metavar "BOOK"
        <> help "location of book metada"))
