#!/usr/bin/env python

"""Use the Baum-Welch algorithm packaged with NLTK to learn the HMM parameters
for rainy days."""

import random
import string
import sys
import math

from numpy import *
from nltk import *
from learncipher import *

from hmm import HiddenMarkovModelTrainer
from collections import defaultdict

def rainydays():
    alphabet = "walk clean".split()
    states = "rainy sunny".split()

    # transitions
    transitions = [[0.6, 0.4], [0.2, 0.8]]
    transitionsArray = array(transitions, float64)
    transitionsDist = conditionalprobdist(transitionsArray, states, states)

    # emissions
    emissions = [[0.5, 0.5], [0.5, 0.5]]
    emissionsArray = array(emissions, float64)
    emissionsDist = conditionalprobdist(emissionsArray, states, alphabet)

    priorsArray = array([0.0,1.0], float64)
    priorsDist = probdist(priorsArray, states)

    model = HiddenMarkovModelTagger(symbols=alphabet, states=states,
                              transitions=transitionsDist,
                              outputs=emissionsDist,
                              priors=priorsDist)

    training = [[("walk", None), ("clean", None)] * 10] * 10
    print training[0]

    # train on those examples, starting with the model that generated them
    trainer = HiddenMarkovModelTrainer(states, alphabet)

    print "OK TRAINING FOR RAINY DAYS!!"
    # hmm = trainer.train_supervised(training)
    hmm = trainer.train_unsupervised(training, model=model,
                              max_iterations=200,
                              convergence_logprob=1e-10)
    return hmm

def days():
    hmm = rainydays()
    print hmm
    print getmappings(hmm)
    print "done"

def main():
    print "o hai"
    days()

if __name__ == "__main__": main()
