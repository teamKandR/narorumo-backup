#!/usr/bin/env python

# We could imagine making features like...
## has internal rhymes
## tends towards certain vowels
## tends towards alliteration/assonance
## repeated sounds: that's easy?
## repeated sounds within a line...
## meter within a line
## don't say a word twice. ("oh my god. I said a word twice.")
### -> that's like "what fraction of the words only happen once?"
## judged likely wrt a particular language model
### -> so you could be sampling words either from the whole vocabulary, or just
###    from the language model. Maybe always do both and see which one wins.
			  
## these things take a list of lines, where each line is a list of words

import random
import copy
import string
from functools import reduce

def flatten(poem):
    return reduce(lambda sofar,line: sofar+line, poem, [])

def astext(poem):
    return " ".join(flatten(poem))

def minimize_repeated_words(poem):
    flattened = flatten(poem) 
    uniquewords = set(flattened)
    return len(uniquewords) / len(flattened)

import rhymes
def last_words_rhyme(poem):
    nlines = len(poem)
    lastwords = [line[-1].lower() for line in poem]

    for word in lastwords:
        otherwords = lastwords[:]
        otherwords.remove(word)
        currhymescore = 0

        for otherword in otherwords:
            currhymescore += rhymes.best_rhyme_score(word, otherword)
    return (currhymescore / nlines)

vowels = "aeiouAEIOU"
def maximize_vowels(poem):
    """Fraction of the letters in the poem that are vowels."""
    text = astext(poem) 
    total = sum( [1 if c in vowels else 0 for c in text] )
    return (total / len(text))

def maximize_alphabeticity(poem):
    """Fraction of words that are in alphabetical order in their line."""
    totalwords = sum([len(line) for line in poem])
    wordsinplace = 0

    for line in poem:
        tosort = copy.deepcopy(line)
        tosort.sort()
        wordsinplace += sum([1 if tosort[i] == line[i] else 0
                               for i in range(len(line))])
    return wordsinplace / totalwords

def prefer_capitals(poem):
    """Give higher scores for words with ALL CAPS."""
    totalwords = sum([len(line) for line in poem])
    allcapscount = 0
    for line in poem:
        for word in line:
            if all([c in string.uppercase for c in word]):
                allcapscount += 1
    return allcapscount / totalwords

def prefer_coherent(poem):
    """Average language model score for each transition in the poem."""
    totalwords = sum([len(line) for line in poem])
    allcapscount = 0
    for line in poem:
        for word in line:
            if all([c in string.uppercase for c in word]):
                allcapscount += 1
