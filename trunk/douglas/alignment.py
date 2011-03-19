#!/usr/bin/env python3

"""
Learn the alignment model. Start out with IBM Model 1.
"""

## So here's how we learn the translation tables. The deep question is, since
## this abstracts over the different possible alignments, how do you get
## alignments explicitly (the most probable ones), for a given bitext
## pair?

"""
Input: set of sentence pairs (e,f)
outout: translation prob t(e | f)
"""

def translation_tables(esentences, fsentences):
    """Given two lists of sentences (each a list of words), build and
    return the translation table t(e|f), which is a dictionary of the shape
    t[french][english] = prob."""

    t = {}
    evocab = set()
    fvocab = set()

    # initialize t(e|f) uniformly
    for eword in evocab:
        for fword in fvocab:
            t[fword] = {}
            t[fword][eword] = 1 / len(evocab)

    # while not converged...
    for step in range(5):
        print("step", step)

        # initialize
        count(e|f) = 0 for all e, f
        total(f) = 0 for all f

        for all sentence pairs(esent,fsent) do:
            # compute normalization
            for all words e in esent do:
                s-total(e) = 0
                for all words f in fsent do:
                    s-total(e) += t(e|f)

            # collect counts
            for all words e in esent do:
                for all words f in fsent:
                    count(e|f) += t(e|f) / s-total(e)
                    total(f) += t(e|f) / s-total(e)

        # estimate probabilities
        for all foreign words f do:
            for all English words e do:
                t(e|f) = count(e|f) / total(f)

import os

englishpath = os.path.expanduser("~/corpora/es-en/europarl-v6.es-en.en")
spanishpath = os.path.expanduser("~/corpora/es-en/europarl-v6.es-en.es")

def getbitext(n):
    with open (englishpath) as enfile:
        with open (spanishpath) as esfile:
            zipped = zip(enfile, esfile)
            return [next(zipped) for i in range(n)]

def main():
    print(getbitext(10))

if __name__ == "__main__": main()
