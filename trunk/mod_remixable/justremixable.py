#!/usr/bin/env python3

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

    # line actually ends at "WITH"
    "AFTER MANY YEARS, STOPS BREATHING, WHILE ASLEEP, WITH" # SMILE ON FACE",
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
]

nonremixable = [
    "FLAMING MARSHMALLOW",
    "ALMOND",
    "VEGETABLES",
    "SHOT BY SNIPER",
    "Contributors", # Arbitrary decision, really. Left in Preface and Intro,
                    # taking out the Contributors. Do those count as stories?
                    # Should they be in the corpus?
]

import sys
import re

def main():
    printing = False
    seenfirstpreface = False
    prevlineblank = False
    digits = re.compile("^(\d+|[xvi]+)$")

    with open(sys.argv[1]) as infile:
        for line in infile:
            if "\x0c" in line: continue
            line = line.strip()

            if re.match(digits, line): continue

            # The line "Preface" shows up twice; we only want the second one.
            if line == "Preface" and not seenfirstpreface:
                seenfirstpreface = True
                continue

            if line in remixable and seenfirstpreface:
                printing = True
            if line in nonremixable:
                printing = False

            if printing:
                if line == "" and prevlineblank:
                    continue
                print(line)

            prevlineblank = (line == "")

if __name__ == "__main__": main()
