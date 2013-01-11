#!/usr/bin/env python3

import re
from collections import defaultdict

speak_pattern = re.compile(r"\d\d:\d\d <.(\S+)> (.*)")

log_fn = "/home/alex/soic.log"

nicks_to_lines = defaultdict(lambda: [])

def lines_said_by(nick):
    if nick in nicks_to_lines:
        return nicks_to_lines[nick]
    with open(log_fn) as infile:
        for line in infile:
            mat = re.match(speak_pattern, line)
            if mat:
                matched_nick, text = mat.groups()
                nicks_to_lines[matched_nick].append(text)
    return nicks_to_lines[nick]

def main():
    by_alexr = lines_said_by("alexr")
    for line in by_alexr:
        print(line)

if __name__ == "__main__": main()
