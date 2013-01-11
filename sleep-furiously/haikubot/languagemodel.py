#!/usr/bin/env python3

import re
from functools import lru_cache

import nltk
import warnings
warnings.filterwarnings('ignore')

import readfromlogs

word_pat = re.compile(r"^\w+$")

@lru_cache(maxsize=100)
def language_model_for(nick):
    """Return a language model for the specified nick, or None if there's not
    much data for that user."""
    lines = readfromlogs.lines_said_by(nick)
    print("got lines")
    sentences = []
    for line in lines:
        tokenized = nltk.word_tokenize(line)
        justwords = list(filter(lambda w: re.match(word_pat, w), tokenized))
        sentences.append(justwords)

    if len(sentences) < 10: return None

    print("got sentences")
    out = nltk.NgramModel(3, sentences)
    print("got lm")
    return out

def main():
    alexr_lm = language_model_for("alexr")
    print(alexr_lm.generate(10))

if __name__ == "__main__": main()
