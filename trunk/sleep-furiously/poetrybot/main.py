#!/usr/bin/env python3

import sys
import random
import copy

import beamsearch
import generate
import poemfeatures

words = set()
def loadallwords():
    global words
    words = open("/usr/share/dict/words").read().split("\n")
loadallwords()

import syllables
def getword():
    out = random.choice(words)

    if out in syllables.PRONUNCIATIONS:
        return out
    else:
        return getword()

class PoemCandidate(beamsearch.Candidate):
    metrics = []

    def __init__(self, lines, model):
        self.val = lines
        self.model = model
        self.score = self.scoreme()

    def __repr__(self):
        return "\n".join( [" ".join(line) for line in self.val] ) + "\n"

    def scoreme(self):
        score = 0
        for (fun,weight) in PoemCandidate.metrics:
            score += (fun(self.val) * weight)
        return score

    def update(self):
        lineindex = random.randint(0, len(self.val) - 1)
        wordindex = random.randint(0, len(self.val[lineindex]) - 1)
        newpoem = copy.deepcopy(self.val)

        ## Regenerate the chosen line starting at the chosen wordindex.
        oldline = newpoem[lineindex]

        if lineindex == 0 and wordindex == 0:
            context = random.sample(self.model._ngrams, 1)[0]
        elif wordindex == 0:
            context = [newpoem[lineindex-1][-1]]
        else:
            context = [newpoem[lineindex][wordindex-1]]

        newwords = self.model.generate(len(oldline) - wordindex, context)[1:]
        newline = oldline[:wordindex] + newwords
        newpoem[lineindex] = newline
        return PoemCandidate(newpoem, self.model)

def naive_poem(model):
    return [generate.line_of_length(5, model),
            generate.line_of_length(7, model),
            generate.line_of_length(5, model)]

def main():
    poems = []

    metrics = PoemCandidate.metrics
    metrics.append((poemfeatures.minimize_repeated_words, 200.0))
    # metrics.append((poemfeatures.maximize_vowels, 5.0))
    # metrics.append((poemfeatures.maximize_alphabeticity, 5.0))
    metrics.append((poemfeatures.last_words_rhyme, 100.0))

    if len(sys.argv) > 1:
        fn = sys.argv[1]
    else:
        fn = "../teb-lisp/wordlist.sexpr"
    model = generate.buildmodel(fn, onlywholewords=True)
    startword = random.sample(model._ngrams, 1)[0][0]

    for i in range(100):
        poems.append(PoemCandidate(naive_poem(model), model))
    better = beamsearch.beamsearch(poems, 100)
    better = sorted(better, key=lambda x: x.score, reverse=True)
    for i in range(5):
        print((better[i], better[i].score))

if __name__ == "__main__": main()
