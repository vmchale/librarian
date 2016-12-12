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

You might also want to use
```stack ghci```
if you want more fine-grained control of things
