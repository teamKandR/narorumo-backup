#!/usr/bin/env python

"""
Generation. Able to take in a bunch of different signals, or will be sooner or
later.
"""

## stuff for ngrams.
## depends on nltk trunk r8590 or later.
import nltk  
import random
from nltk.model.ngram import NgramModel

from syllables import count_syllables, word_to_syllables

def line_of_length(nsyllables, model, context=[]):
    """Generate a line with nsyllables syllables via recursive search."""

    for i in range(10):
        if not context:
            candidate  = random.sample(model._ngrams, 1)[0][0]
        else:
            candidate = model.choose_random_word(context)

        candidatelen = count_syllables(candidate.lower())

        if candidatelen == nsyllables:
            return [candidate]
        elif candidatelen > nsyllables:
            continue
        else:
            searchfurther = line_of_length(nsyllables - candidatelen,
                                           model,
                                           context + [candidate])
            if searchfurther:
                proposed = [candidate] + searchfurther
                return proposed
    return None

def count_line_syllables(words):
    return sum( map(count_syllables, words) )

def buildmodel(textfn, onlywholewords=False):
    """Takes a filename for some input text, tokenizes, and builds a bigram
    model."""
    text = open(textfn, "r").read()
    words = nltk.word_tokenize(text)

    if onlywholewords:
        import string
        isletter = lambda c: c in string.letters
        words = [word for word in words if all(map(isletter, word))]

    model = NgramModel(2, words)

    return model

def main():
    model = buildmodel("../teb-lisp/wordlist.sexpr")
    startword = random.sample(model._ngrams, 1)[0][0]

    print(" ".join(line_of_length(5, model)))
    print(" ".join(line_of_length(7, model)))
    print(" ".join(line_of_length(5, model)))

if __name__ == "__main__": main()
