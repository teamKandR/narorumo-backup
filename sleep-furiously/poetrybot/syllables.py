#!/usr/bin/env python3

"""
Routines that deal with syllables particularly.
"""

from collections import defaultdict
import string

from nltk.corpus import cmudict

import syllables_en
import utils

PRONUNCIATIONS = defaultdict(lambda:[])
def load_pronouncing():
    """Load up the CMU Pronouncing Dictionary."""
    global PRONUNCIATIONS
    PRONUNCIATIONS = defaultdict(lambda:[], cmudict.dict())
load_pronouncing()
print("Got cmudict.")

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

def sounds_to_syllables(sounds):
    """Take a list of sounds (a flat list of sounds) and produce a list of
    lists of sounds -- each sublist, we consider a syllable."""

    getclosest = lambda pos: closest_vowel_index(sounds, pos)

    # indices of the closest vowel for the sound at the corresponding index
    closests = list(map(getclosest, list(range(len(sounds)))))
    syllables_indexes = list(set(closests))

    return [[sounds[i] for i in range(len(sounds))
                       if closests[i] == syl_num]
                       for syl_num in syllables_indexes]

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

def count_syllables_phonemes(phonemes):
    return len(list(filter(isvowel, phonemes)))

def count_syllables(word):
    """Given a a word as a string, return the number of syllables in that word,
    as best we can."""
    if word in PRONUNCIATIONS:
        try:
            return count_syllables_phonemes(PRONUNCIATIONS[word][0])
        except: pass
    return syllables_en.count(word)

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

def vowel_stress(sound):
    """Get the stress digit off of a vowel."""
    assert isvowel(sound)
    return int(sound[-1])

def vowel_sound(sound):
    """Remove the stress digit from a vowel."""
    assert isvowel(sound)
    return sound[:-1]

def main():
    print(PRONUNCIATIONS["food"])
    print(word_to_syllables("food"))
    print(word_to_syllables("dude"))
    print(word_to_syllables("tomato"))
    print(count_syllables("tomato"))
    print(count_syllables("clee"))

if __name__ == "__main__": main()
