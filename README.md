##Install
First, you'll need to get the C library `libqrencode` from [here](https://github.com/fukuchi/libqrencode)

Then type `stack install` in the appropriate library.

To make sure bash completions run correctly, run `bash/mkCompletions` as well. You might also have to do

```
mkdir -p db/labels
mkdir db/cards
mkdir hamlet/cards
```

to ensure the right folders are in place

##Config
First, find the file called `.pw` and edit it appropriately

##Usage
To help you get started, there is a bash file called `examples` in the appropriate directory, which has many of the common commands you will issue. 

You might also want to use
```stack ghci```
if you want more fine-grained control of things
