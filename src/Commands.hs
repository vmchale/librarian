module Options ( exec
               ) where

import Options.Applicative
import LibraryDB
import Types

data Program = Program
    { card  :: FilePath --if left blank, stdin
    , book' :: FilePath --if left blank, stdout
    , json  :: Bool --use json, else .png
    , com   :: Com }

data Com = AddBook { title' :: String , author' :: String } --output as author-title-(year).png
               | NewPatron { name' :: String, email' :: String }
               | Checkout
               | Return --patron should be optional here!
               | PrintCard { email' :: String } --print card as email.png
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
            <> progDesc "Manage a library via json-poopDB and make library cards"
            <> header "library - a home library manager"
            )

pick :: Program -> IO ()
pick (Program _ _ True (PrintCard ema)) = (fmap (head . filter (\a -> (==) (_email a) (ema)))) getPatrons >>= mkCard
pick (Program _ _ _ _) = putStrLn "idk it failed"

program :: Parser Program
program = Program
    <$> strOption
        (long "card"
        <> metavar "CARD"
        <> help "location of library card file")
    <*> strOption
        (long "book"
        <> metavar "BOOK"
        <> help "location of book metada")
    <*> switch
        (long "json"
        <> help "toggles json or QR codes")
    <*> subparser
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
            )

        -- <> command "add-book" (info (pure $ AddBook "a" "B")
        --    (progDesc "Add a book to the db."))
