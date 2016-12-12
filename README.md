##Install
First, you'll need to get the C library `libqrencode` from [here](https://github.com/fukuchi/libqrencode)

Then type `stack install` in the appropriate library.

To make sure bash completions run correctly, run any command with the `--install` flag. 

##Config
First, find the file called `.pw` and edit it appropriately so that you can send reminder emails to delinquent patrons. It is formatted like so:

```
server: smtp.domain.org
port: 465
email: name@doman.org
passord: *******
```

##Usage
To help you get started, there is a bash file called `examples` in the appropriate directory, which has many of the common commands you will issue. 

###Example
To add a user and print/view their library card:

```
 $ mkdir -p hamlet/cards
 $ library add-user -u "Vanessa McHale" -e "tmchale@wisc.edu" -j
{
    "_record": [],
    "_name": "Vanessa McHale",
    "_email": "tmchale@wisc.edu"
}
 $ library print-card -e "tmchale@wisc.edu"
()
 $ library new-book --title "War and Peace" --author "Leo Tolstoy"
 $ library new-book --title "Memoirs" --author "Vanessa McHale" -j 
{
    "_publicationYear": null,
    "_author": "Vanessa McHale",
    "_checkoutLength": 21,
    "_isbn": null,
    "_publisher": null,
    "_title": "Memoirs"
}
 $ library qrgen
()
QR codes generated successfully.
already generated, skipping...
 $ gnome-open hamlet/cards/tmchale.html
```

###More depth
You might also want to use
```stack ghci```
if you want more fine-grained control of things
