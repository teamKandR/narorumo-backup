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

from collections import defaultdict
from operator import itemgetter

def translation_tables(esentences, fsentences):
    """Given two lists of sentences (each a list of words), build and
    return the translation table t(e|f), which is a dictionary of the shape
    t[e][f] = prob."""

    t = {}
    evocab = set()
    fvocab = set()

    # build e vocab
    for sent in esentences:
        for e in sent.split():
            evocab.add(e)
    print("got {0} english words.".format(len(evocab)))
    # build f vocab
    for sent in fsentences:
        for f in sent.split():
            fvocab.add(f)
    print("got {0} foreign words.".format(len(fvocab)))

    # initialize t(e|f) uniformly
    lowprob = 1 / len(evocab)
    ewords = 0
    for eword in evocab:
        t[eword] = defaultdict(lambda:lowprob)
        # for fword in fvocab:
        #     t[eword][fword] = lowprob
        if ewords % 1000 == 0: print(ewords,eword)
        ewords += 1

    # while not converged...
    for step in range(5):
        print("step", step)

        # initialize
        # count(e|f) = 0 for all e, f
        count = defaultdict(lambda: 0)

        # total(f) = 0 for all f
        total = defaultdict(lambda: 0)

        s_total = {}
        # for all sentence pairs(esent,fsent) do:
        for (esent, fsent) in zip(esentences, fsentences):
            # compute normalization
            for e in esent.split():
                s_total[e] = 0
                for f in fsent.split():
                    s_total[e] += t[e][f]

            # collect counts
            for e in esent.split():
                for f in fsent.split():
                    count[(e,f)] += t[e][f] / s_total[e]
                    total[f] += t[e][f] / s_total[e]

        # estimate probabilities
        for f in fvocab:
            for e in evocab:
                if f in total:
                    t[e][f] = count[(e,f)] / total[f]
                else:
                    t[e][f] = 0.0 ## need smoothing?
    return t

import os

englishpath = os.path.expanduser("~/corpora/es-en/europarl-v6.es-en.en")
spanishpath = os.path.expanduser("~/corpora/es-en/europarl-v6.es-en.es")

def getbitext(n):
    esentences = []
    fsentences = []
    with open (englishpath) as enfile:
        for i in range(n):
            esentences.append(enfile.readline())
    with open (spanishpath) as esfile:
        for i in range(n):
            fsentences.append(esfile.readline())

    return esentences, fsentences

def repl(trans):
    while True:
        print("English word? ", end="")
        e = input().strip()

        if e in trans:
            translations = trans[e].items()
            inorder = sorted(translations, key=itemgetter(1), reverse=True)
            print(inorder[:5])
        else:
            print("don't know {0}".format(e))

def main():
    esentences, fsentences = getbitext(500)
    trans = translation_tables(esentences, fsentences)
    repl(trans)

if __name__ == "__main__": main()
