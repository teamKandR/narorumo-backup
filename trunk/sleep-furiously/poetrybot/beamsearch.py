#!/usr/bin/env python3

"""
Routines for beamsearch.
"""


words = set()
def loadallwords():
    global words
    words = open("/usr/share/dict/words").read().split("\n")
loadallwords()

class Candidate(object):
    def __init__(self, val):
        self.val = val
        self.score = self.scoreme()

    def update(self):
        raise Exception("not implemented")

    def scoreme(self):
        raise Exception("not implemented")

import random
from functools import reduce
class NumberCandidate(Candidate):
    def __repr__(self):
        return str(self.val)
    def scoreme(self):
        return self.val
    def update(self):
        return NumberCandidate(self.val + random.randint(-10,5))

vowels = "aeiouAEIOU"
class LineThatWantsVowels(Candidate):
    """List of five words that wishes it had a lot of vowels in it."""

    def __repr__(self):
        return " ".join(self.val)

    def scoreme(self):
        text = "".join(self.val)
        total = reduce(lambda sofar,c: (sofar + 1) if c in vowels else sofar,
                       text,
                       0)
        # return 1.0 - abs((total / len(text)) - 0.5)
        return (total / len(text))

    def update(self):
        newlist = self.val[:]
        index = random.randint(0,len(newlist) - 1)
        newlist[index] = random.choice(words)
        return LineThatWantsVowels(newlist)

def beamsearch(candidates, maxiter=100):
    """Given:
    candidates: an initial list of candidates.
    terminating: function from a list of candidates to True/False. End if True.

    Do a beam search; return the list of candidates when done.
    """
    for i in range(maxiter):
        newones = [c.update() for c in candidates]

        candidates = [newones[i] if (newones[i].score >= candidates[i].score)
                                 else candidates[i]
                      for i in range(len(candidates))]

    # different search algorithm: is this new poem better than the average old
    # one?

    # consider: how to keep descendents of the top candidate from crowding
    # out everything else? Is that going to be a problem? Maybe not with a
    # wider beam. Maybe we just want to encourage some missteps sometimes.
    # What we're doing here is really just parallelized hillclimbing, not
    # really a proper beamsearch. Maybe like -- randomized restarts, or a
    # benefit for being different from the other candidates?
    # candidates = allcandidates[:len(candidates)]

    candidates.sort(key = lambda c: c.score, reverse=True)
    return candidates

def demo():
    lines = []
    for i in range(100):
        lines.append([random.choice(words),
                      random.choice(words),
                      random.choice(words),
                      random.choice(words),
                      random.choice(words)])
    linecandidates = [LineThatWantsVowels(c) for c in lines]
    better = beamsearch(linecandidates, 2000)

    for i in range(10):
        print((better[i], better[i].score))

if __name__ == "__main__": demo()
