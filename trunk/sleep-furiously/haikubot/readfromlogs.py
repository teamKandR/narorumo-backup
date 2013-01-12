#!/usr/bin/env python3

import re
import nltk
from collections import defaultdict

speak_pattern = re.compile(r"\d\d:\d\d <.(\S+)> (.*)")

log_fn = "/home/alex/soic.log"

nicks_to_sentences = defaultdict(lambda: [])

lines_loaded = False
def load_all_lines():
    """Load all the lines from the file, put them in a dictionary, then strip
    out the initial usernames."""
    unfiltered_nicks_to_lines = defaultdict(lambda: [])

    with open(log_fn) as infile:
        for line in infile:
            mat = re.match(speak_pattern, line)
            if mat:
                matched_nick, text = mat.groups()
                unfiltered_nicks_to_lines[matched_nick].append(text)

    nicks = list(unfiltered_nicks_to_lines.keys())
    pat = "^(" + "|".join(nicks) + ")[,:] "
    p = re.compile(pat)
    for nick in nicks:
        for text in unfiltered_nicks_to_lines[nick]:
            m = re.match(p, text)
            if m:
                text = text[m.span()[1]:]
            sentences = nltk.sent_tokenize(text)
            for sent in sentences:
                nicks_to_sentences[nick].append(sent)

if not lines_loaded:
    load_all_lines()
    lines_loaded = True

def lines_said_by(nick):
    return nicks_to_sentences[nick]

def main():
    by_alexr = lines_said_by("alexr")
    for line in by_alexr:
        print(line)

if __name__ == "__main__": main()
