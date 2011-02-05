#!/usr/bin/env python3

"""
Script to pull out the Creative Commons (with derivs allowed) stories from the
Machine of Death collection. It should have already been preprocessed by the
rest of the accompanying shell script, makeremixable.sh.

by Alex Rudnick (alex.rudnick at gmail dot com)
"""

## Optional; maybe you don't want Preface, Introduction and Contributors in
## your training data. But David Malki ! says it's fine to include them, so I
## did. I did decide, though, that I don't want the Copyright notice at the end
## in my training data.
remixable = [
    "Preface",
    "Introduction",
    "FUDGE",
    "TORN APART AND DEVOURED BY LIONS",
    "DESPAIR",
    "SUICIDE",
    "STARVATION",
    "CANCER",
    "FIRING SQUAD",
    "PIANO",
    "HIV INFECTION FROM MACHINE OF DEATH NEEDLE",
    "EXPLODED",
    "NOT WAVING BUT DROWNING",
    "IMPROPERLY PREPARED BLOWFISH",
    "LOVE AD NAUSEUM",
    "MURDER AND SUICIDE, RESPECTIVELY",
    "CANCER",
    "ANEURYSM",
    "EXHAUSTION FROM HAVING SEX WITH A MINOR",
    "AFTER MANY YEARS, STOPS BREATHING, WHILE ASLEEP, WITH SMILE ON FACE",
    "KILLED BY DANIEL",
    "FRIENDLY FIRE",
    "NOTHING",
    "COCAINE AND PAINKILLERS",
    "LOSS OF BLOOD",
    "PRISON KNIFE FIGHT",
    "WHILE TRYING TO SAVE ANOTHER",
    "MISCARRIAGE",
    "HEAT DEATH OF THE UNIVERSE",
    "DROWNING",
    "?",
    "CASSANDRA",
    "Contributors"
]

nonremixable = [
    "FLAMING MARSHMALLOW",
    "ALMOND",
    "VEGETABLES",
    "SHOT BY SNIPER",
    "Copyright",
]

import sys
import re

def main():
    printing = False
    seenfirstsection = False
    prevlineblank = False

    sectionsfound = []

    with open(sys.argv[1]) as infile:
        for line in infile:
            if "\x0c" in line: continue
            line = line.strip()

            if line == "###":
                seenfirstsection = True
                continue

            if line in remixable and seenfirstsection:
                printing = True
                sectionsfound.append(line)

            if line in nonremixable:
                printing = False

            if printing:
                if line == "" and prevlineblank:
                    continue
                print(line)

            prevlineblank = (line == "")
    assert(sectionsfound == remixable)

if __name__ == "__main__": main()
