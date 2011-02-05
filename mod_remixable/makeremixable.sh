#!/bin/bash

## Assumes that MachineofDeath_FINAL.pdf is here.
## Requires: pdftotext, iconv and Python 3.

# Convert from pdf to unicode.
pdftotext MachineofDeath_FINAL.pdf step1.txt

# Now unicode to flat ascii.
iconv -f utf8 -t ascii//TRANSLIT step1.txt > step2.txt

# Now fire up the Python script to take out the linefeeds, page numbers, and
# just leave the stories that are CC-BY-NC-SA.
python3 ./justremixable.py step2.txt > mod_remixable.txt

# Clean up the intermediate steps.
rm -f step1.txt step2.txt
