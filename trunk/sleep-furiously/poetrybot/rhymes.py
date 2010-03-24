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

from __future__ import division
from nltk.corpus import cmudict
import utils
import string

PRONUNCIATIONS = {}

POINTS_VOWEL_SOUND = 8
POINTS_VOWEL_STRESS = 1
POINTS_SUFFIX = 6
POINTS_PREFIX = 3

def load_pronouncing():
    """Load up the CMU Pronouncing Dictionary."""
    global PRONUNCIATIONS
    PRONUNCIATIONS = cmudict.dict()
    print len(PRONUNCIATIONS)

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
    return max(scores)

def best_rhyming_word(word, possibilities):
    """For the given word (as a string), and all the words in the possibilities
    list, pick out the one with the highest rhyme score."""

    score = lambda candidate: best_rhyme_score(word, candidate)
    word_scores = [(poss, score(poss)) for poss in possibilities]

    thekey = lambda x: x[1]
    bestpair = max(word_scores, key=thekey)
    return bestpair[0]

def isvowel(sound):
    """Given a string representing a sound, we call it a vowel sound if it ends
    with a number -- indicating a stress value in the pronouncing dictionary
    markup."""
    return sound[-1] in string.digits

def syllable_prefix(syllable):
    """Return the part of the syllable before the vowel sound."""
    index = 0
    for phone in syllable:
        if isvowel(phone):
            break
        else:
            index += 1

    return syllable[:index]

def syllable_suffix(syllable):
    """Part of the syllable after the vowel sound."""
    index = 1
    for phone in syllable:
        if isvowel(phone):
            break
        else:
            index += 1

    return syllable[index:]

def remove_stress(sound):
    """Given a phoneme, take out the stress indicator on it."""
    for digit in "012":
        if sound.endswith(digit):
            return sound[:-1]
    return sound

def count_syllables(phonemes):
    if isinstance(phonemes,basestring): # got a word, not a list of syllables!
        word = phonemes
        return count_syllables(PRONUNCIATIONS[words][0])
    else:
        return len( filter(isvowel, phonemes) )

def closest_vowel_index(sounds, pos):
    """Given a list of sounds and an index within that sound (the position of
    the consonant we're interested in), find out which vowel sound is the
    closest. In the event of a tie, pick the earlier one."""

    if isvowel(sounds[pos]):
        return pos

    pairs = [(index, abs(pos - index)) for index in range(len(sounds))
                                        if isvowel(sounds[index])]
    thekey = lambda x: x[1]
    bestpair = min(pairs, key=thekey)
    return bestpair[0]

def vowel_stress(sound):
    """Get the stress digit off of a vowel."""
    assert isvowel(sound)
    return int(sound[-1])

def vowel_sound(sound):
    """Remove the stress digit from a vowel."""
    assert isvowel(sound)
    return sound[:-1]

def sounds_to_syllables(sounds):
    """Take a list of sounds (a flat list of sounds) and produce a list of
    lists of sounds -- each sublist, we consider a syllable."""

    getclosest = lambda pos: closest_vowel_index(sounds, pos)

    # indices of the closest vowel for the sound at the corresponding index
    closests = map(getclosest, range(len(sounds)))
    syllables_indexes = list(set(closests))

    return [[sounds[i] for i in range(len(sounds))
                       if closests[i] == syl_num]
                       for syl_num in syllables_indexes]

def word_to_syllables(word):
    return [ sounds_to_syllables(sounds) for sounds in PRONUNCIATIONS[word] ]

def syllable_stress(syllable):
    """Given a syllable (list of strings), return its stress."""
    for sound in syllable:
        if isvowel(sound):
            return vowel_stress(sound)
    raise ValueError("Should have had a vowel in that syllable?")

def syllable_vowel_sound(syllable):
    """Given a syllable (list of sound strings) return the sound of its vowel;
    this will be just the vowel, without the stress."""
    for sound in syllable:
        if isvowel(sound):
            return vowel_sound(sound)
    raise ValueError("Should have had a vowel in that syllable?")
			  
def main():
    load_pronouncing()
    print PRONUNCIATIONS["food"]

    print word_to_syllables("computer")
    print word_to_syllables("disregard")
    print best_rhyming_word("felon", ["flea", "computer", "melon", "egress"])

if __name__ == "__main__": main()
