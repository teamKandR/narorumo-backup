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
from __future__ import division
import random
import copy

from beamsearch import beamsearch
from beamsearch import Candidate

words = set()
def loadallwords():
    global words
    words = open("/usr/share/dict/words").read().split("\n")
loadallwords()

def getword():
    return random.choice(words)

def flatten(poem):
    return reduce(lambda sofar,line: sofar+line, poem, [])

def astext(poem):
    return " ".join(flatten(poem))

def minimize_repeated_words(poem):
    flattened = flatten(poem) 
    uniquewords = set(flattened)
    return len(uniquewords) / len(flattened)

vowels = "aeiouAEIOU"
def maximize_vowels(poem):
    text = astext(poem) 
    total = reduce(lambda sofar,c: (sofar + 1) if c in vowels else sofar,
                   text,
                   0)
    return (total / len(text))

def maximize_alphabeticity(poem):
    """Fraction of words that are in alphabetical order in their line."""
    totalwords = sum([len(line) for line in poem])
    wordsinplace = 0

    for line in poem:
        tosort = copy.deepcopy(line)
        tosort.sort()
        wordsinplace += sum([1 if tosort[i] == line[i] else 0
                               for i in xrange(len(line))])
    return wordsinplace / totalwords

class PoemCandidate(Candidate):
    def __repr__(self):
        return "\n".join( [" ".join(line) for line in self.val] ) + "\n"

    def scoreme(self):
        return maximize_vowels(self.val)
        # return (minimize_repeated_words(self.val) + 
        #         maximize_vowels(self.val) + 
        #         3.0 * maximize_alphabeticity(self.val))

    def update(self):
        lineindex = random.randint(0, len(self.val) - 1)
        wordindex = random.randint(0, len(self.val[lineindex]) - 1)
        newpoem = copy.deepcopy(self.val)
        newpoem[lineindex][wordindex] = getword()
        return PoemCandidate(newpoem)

def naive_poem():
    return [[getword(), getword(), getword(), getword(), getword()],
            [getword(), getword(), getword(), getword(), getword()],
            [getword(), getword(), getword(), getword(), getword()]]

def main():
    poems = []
    for i in xrange(10):
        poems.append(PoemCandidate(naive_poem()))
    better = beamsearch(poems, 200)
    for i in xrange(5):
        print better[i], better[i].score

if __name__ == "__main__": main()
