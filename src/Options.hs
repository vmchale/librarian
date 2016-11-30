module Options ( exec
               ) where

import Options.Applicative
import LibraryDB
import Types
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson

data Program = Program
    { com   :: Com --if left blank, stdin
    , json  :: Bool --if left blank, stdout
    , card  :: FilePath --use json, else .png
    , book' :: FilePath }

data Com = AddBook { title' :: String , author' :: String }
           | NewPatron { name' :: String, email' :: String }
           | Checkout
           | Return
           | PrintCard { email' :: String }
--with subcommands I could have lenses for my applicative parser combinators? wtf.

exec' :: IO ()
exec' = do
    createBook $ newBook "In pursuit of pure reason" "Vanessa McHale"
    newPatron $ createPatron "Vanesa McHale" "tmchale@wisc.edu"
    (checkout (createPatron "The" "a@a.com") (newBook "In pursuit of pure reason" "Vanessa McHale")) >>= newPatron
    showBooks
    --updateQR

exec :: IO ()
exec = execParser opts >>= pick
    where
        opts = info (helper <*> program)
            (fullDesc
            <> progDesc "Manage a library via json-poopDB and make library cards" --one way to serialize database access: make a monad? unlike io the monoidal addition acctually sequences it weird.
            <> header "library - a home library manager"
            )

pick :: Program -> IO ()
pick (Program (PrintCard ema) False _ _) = (fmap (head . filter (\a -> (==) (_email a) (ema)))) getPatrons >>= mkCard
pick (Program Checkout True patf boof) = do
    let pat = (fmap head) $ getRecord patf
    let boo = (fmap head) $ getRecord boof
    p' <- checkout <$> pat <*> boo
    p' >>= updatePatron
pick (Program (AddBook tit aut) True _ _) = do 
    let boo = newBook tit aut
    createBook boo
    showObject boo
pick (Program Return True _ boof) = do
    let boo = (fmap head) $ getRecord boof
    let p' = return' <$> (boo >>= patronByBook) <*> boo
    p' >>= updatePatron
pick (Program (NewPatron nam ema) True _ _) = do 
    let pat = createPatron nam ema
    newPatron pat
    showObject pat
pick (Program (NewPatron nam ema) False _ _) = do 
    let boo = newBook nam ema
    createBook boo
    updateQR
    putStrLn "...qr code written to file"

program :: Parser Program
program = Program
    <$> subparser
            (command "checkout" (info (pure Checkout)
                (progDesc "Check out a book."))
            <> command "return" (info (pure Return)
                (progDesc "Return out a book."))
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
    <*> strOption
        (long "card"
        <> short 'u'
        <> metavar "CARD"
        <> help "location of library card file")
    <*> strOption
        (long "book"
        <> metavar "BOOK"
        <> help "location of book metada")