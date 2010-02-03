#!/usr/bin/env python

"""
Routines for rhyming.

Based on an algorithm originally by Bradley Buda:
http://www.bradleybuda.com/rhymes/
"""

from nltk.corpus import cmudict

sounds = {}

def load_pronouncing():
    """Load up the CMU Pronouncing Dictionary."""
    global sounds
    sounds = cmudict.dict()
    print len(sounds)

def main():
    load_pronouncing()
    print sounds["food"]

if __name__ == "__main__": main()
