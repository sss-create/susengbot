# susengbot
Simple Telegram bot for displaying cute cat images
written in Haskell.

Find the channel here: https://t.me/suseng_channel

# Documentation
Use with private bot token in ```data/token.txt```.
Create a Database in ```data/```.
Insert the desired ```chat_id``` in ```sendPhoto```.

susengbot uses SQLite3 for storing image id and url.
The Database table is created via the following command:

```
CREATE TABLE cats (
id text primary key,
url text,
date_added DATETIME default current_timestamp);
```

compile and add a cronjob for a daily picture:
```
16 11 * * * cd susengbot && ./susengbot
```

Developed/Tested with:
```
ghc 9.4.5
cabal 3.6.2.0
SQLite 3.40.1-2 
zlib1g-dev version 1:1.2.13
gcc-12.2.0-14
```
