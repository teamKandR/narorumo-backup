#!/usr/bin/env python

"""
Routines for rhyming.

Based on an algorithm originally by Bradley Buda:
http://www.bradleybuda.com/rhymes/

Overview:
We want to quantify how much two syllables rhyme. When we look up the
pronunciation of a word, we get a list of phonemes. (these might be vowels,
with a stress marker, or consonants).
We then break the word into syllables; a syllable is also a list of phonemes,
but there might be several per word.
"""


import string

from syllables import word_to_syllables
from syllables import syllable_prefix, syllable_suffix
from syllables import syllable_stress, syllable_vowel_sound

import utils

POINTS_VOWEL_SOUND = 8
POINTS_VOWEL_STRESS = 1
POINTS_SUFFIX = 6
POINTS_PREFIX = 3

def rhyme_score(sylA, sylB):
    """Given two syllables, return their rhyme score, from 0 to 1."""
    scored = 0
    possible = (POINTS_VOWEL_STRESS + POINTS_VOWEL_SOUND + POINTS_PREFIX +
                POINTS_SUFFIX)

    prefixA = syllable_prefix(sylA)
    prefixB = syllable_prefix(sylB)
    suffixA = syllable_suffix(sylA)
    suffixB = syllable_suffix(sylB)

    if syllable_stress(sylA) == syllable_stress(sylB):
        scored += POINTS_VOWEL_STRESS

    if syllable_vowel_sound(sylA) == syllable_vowel_sound(sylB):
        scored += POINTS_VOWEL_SOUND

    scored += soundmatch(prefixA, prefixB, POINTS_PREFIX)
    scored += soundmatch(suffixA, suffixB, POINTS_SUFFIX)

    return (scored / possible)

def soundmatch(sounda, soundb, maxpoints):
    """Given two sounds (suffixes or prefixes), calculate a score for their
    similarity."""

    ## if they're both not zero
    if len(sounda) == 0 and len(soundb) == 0:
        return 0

    frac = utils.levenshtein(sounda, soundb) / max(len(sounda), len(soundb))
    return maxpoints * (1 - frac)

def best_rhyme_score(word1, word2):
    """For all pronunciations of word1 and word2, find and return the highest
    rhyme score."""

    scores = []
    for syls1 in word_to_syllables(word1):
        for syls2 in word_to_syllables(word2):
            scores.append( rhyme_score(syls1[-1], syls2[-1]) )
    if scores:
        return max(scores)
    return 0.0

def best_rhyming_word(word, possibilities):
    """For the given word (as a string), and all the words in the possibilities
    list, pick out the one with the highest rhyme score."""

    score = lambda candidate: best_rhyme_score(word, candidate)
    word_scores = [(poss, score(poss)) for poss in possibilities]

    thekey = lambda x: x[1]
    bestpair = max(word_scores, key=thekey)
    return bestpair[0]

def main():
    print((best_rhyming_word("felon", ["flea", "computer", "melon", "egress"])))
    print((best_rhyming_word("egret", ["flea", "regret", "melon", "egress"])))

if __name__ == "__main__": main()
