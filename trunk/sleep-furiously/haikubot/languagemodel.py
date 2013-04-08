#!/usr/bin/env python3

import re
from functools import lru_cache

import nltk
import warnings
warnings.filterwarnings('ignore')

import readfromlogs

word_pat = re.compile(r"^\w+$")
contraction_pat = re.compile(r"^[A-Za-z']+$")

def contraction(w):
    return (("'" in w) and
            (len(w) in range(2,4)) and
            (w.count("'") == 1) and
            bool(re.match(contraction_pat, w)))

def word_or_contraction(w):
    return (contraction(w) or is_word(w))

def is_word(w):
    return bool(re.match(word_pat, w))

def merge_contractions(sent):
    s = ""
    for token in sent:
        if not contraction(token):
            s += " "
        s += token
    return s.split()

@lru_cache(maxsize=100)
def language_model_for(nick):
    """Return a language model for the specified nick, or None if there's not
    much data for that user."""
    lines = readfromlogs.lines_said_by(nick)
    print("got lines")
    sentences = []
    for line in lines:
        tokenized = nltk.word_tokenize(line)
        justwords = list(filter(word_or_contraction, tokenized))

        joined = merge_contractions(justwords)
        sentences.append(joined)

    if len(sentences) < 10: return None

    print("got sentences")
    out = nltk.NgramModel(3, sentences)
    print("got lm")
    return out

def main():
    print(contraction("n't"))
    print(contraction("'ll"))
    print(contraction("food"))
    alexr_lm = language_model_for("alexr")
    print(alexr_lm.generate(10, context=["can't"]))

if __name__ == "__main__": main()
