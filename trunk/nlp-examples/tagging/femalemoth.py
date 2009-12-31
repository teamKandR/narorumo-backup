#!/usr/bin/env python

"""Use the Viterbi algorithm to do part of speech tagging for the sentence "a
myth is a female moth", and also the Time Flies sentence."""

import random
import string
from collections import defaultdict

import hmm_trellis as fb

def timeflies():
    print """Tagging: "time flies like an arrow"."""
    sentence = "time flies like an arrow".split()

    def zero(): return 0.0

    states = ["NN","VB", "JJ", "VBZ", "NNS", "IN", "RB", "DT"]
    emissions = { "NN" : defaultdict(zero, {"time": .070727,
                                            "arrow": 0.000215}),
                  "VB" : defaultdict(zero, {"time": 0.000005,
                                      "like": .028413}),
                  "JJ" : defaultdict(zero, {"time": 0.0}),
                  "VBZ": defaultdict(zero, {"flies": 0.004754}),
                  "NNS" : defaultdict(zero, {"flies": 0.001610}),
                  "IN" : defaultdict(zero, {"like": .026512}),
                  "RB" : defaultdict(zero, {"like": 0.005086}),
                  "DT": defaultdict(zero, {"an": .014192})}

    initial = defaultdict(zero,
                {"NN": 0.006823,
                 "VB": 0.005294,
                 "JJ": 0.008033})

    transitions = {
      "NN": defaultdict(zero,
             {"VBZ":.039005,
             "NNS":.016076}),
      "VB" : defaultdict(zero,
              {"VBZ": 0.000566,
              "NNS": 0.006566,
              "DT": .152649}),
      "JJ" : defaultdict(zero,
             {"VBZ": .020934,
             "NNS": .024383}),
      "VBZ": defaultdict(zero,
              {"IN": .085862,
              "VB": 0.007002,
              "RB": .150350}),
      "NNS": defaultdict(zero,
              {"IN": .218302,
              "VB": .111406,
              "RB":.064721}),
      "IN" : defaultdict(zero, {"DT": .314263}),
      "RB" : defaultdict(zero, {"DT": .053113}), 
      "DT": defaultdict(zero, {"NN": .380170})
    }

    trellis = fb.Trellis(sentence, transitions, emissions, initial,
        states=states)
    print trellis.best_path()

def femalemoth():
    print """Tagging: "a myth is a female moth"."""
    sentence = "a myth is a female moth".split()

    initial = {
        "DT": 0.45,
        "JJ": 0.35,
        "NN": 0.15,
        "VB": 0.05
        }

    transitions = {
        "DT": {"DT":0.03, "JJ":0.42, "NN":0.50, "VB":0.05},
        "JJ": {"DT":0.01, "JJ":0.25, "NN":0.65, "VB":0.09},
        "NN": {"DT":0.07, "JJ":0.03, "NN":0.15, "VB":0.75},
        "VB": {"DT":0.30, "JJ":0.25, "NN":0.15, "VB":0.30}
    }

    emissions = {
        "DT": {"a":0.85, "myth":0.01, "is":0.02, "female":0.01, "moth":0.12},
        "JJ": {"a":0.05, "myth":0.10, "is":0.02, "female":0.60, "moth":0.13},
        "NN": {"a":0.03, "myth":0.45, "is":0.02, "female":0.25, "moth":0.25},
        "VB": {"a":0.05, "myth":0.10, "is":0.60, "female":0.05, "moth":0.20}
    }

    trellis = fb.Trellis(sentence, transitions, emissions, initial)
    print trellis.best_path()

def main():
    femalemoth()
    timeflies()

if __name__ == "__main__": main()
