#!/bin/bash

## Assumes that mod.rtf is here.
## That file is available at: http://machineofdeath.net/mod.rtf
## Requires: unoconv, iconv and Python 3, all of which are available on modern
## Linuxes, such as for instance Ubuntu.

# Convert from rtf to unicode.
# pdftotext MachineofDeath_FINAL.pdf step1.txt
killall -9 soffice.bin
unoconv --format=text mod.rtf
mv mod.txt step1.txt
echo "OK converted rtf->unicode... "

# Now unicode to flat ascii.
iconv -f utf8 -t ascii//TRANSLIT step1.txt > step2.txt
echo "OK converted unicode->ascii..."
# Now fire up the Python script to take out the linefeeds, page numbers, and
# just leave the stories that are CC-BY-NC-SA.

echo "OK pulling out the remixable stories..."
python3 ./justremixable.py step2.txt > mod_remixable.txt

echo "OK cleaning up..."
# Clean up the intermediate steps.
rm -f step1.txt step2.txt

echo "OK your remixable corpus should be in mod_remixable.txt. Share and enjoy!"
