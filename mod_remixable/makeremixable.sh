#!/bin/bash

## Assumes that mod.rtf is here.
## That file is available at: http://machineofdeath.net/mod.rtf
## Requires: unoconv, iconv and Python 3, all of which are available on modern
## Linuxes, such as for instance Ubuntu.

# Convert from rtf to unicode.
killall -9 soffice.bin
unoconv --format=text mod.rtf
mv mod.txt step1.txt
echo "OK converted rtf->unicode... "

iconv -f utf8 -t ascii//TRANSLIT step1.txt > step2.txt
echo "OK converted unicode->ascii..."

echo "OK pulling out the remixable stories..."
python3 ./justremixable.py step2.txt > mod_remixable.txt

echo "OK cleaning up..."
rm -f step1.txt step2.txt

echo "OK your remixable corpus should be in mod_remixable.txt. Share and enjoy!"
