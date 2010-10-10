#!/usr/bin/env python

"""
Naive bayes classification.

Takes a training set and a test set, each as a csv file, and learns a naive
bayes classifier based on the training set. Then run the classifier against
each member of the test set and print out the accuracy.

For the values in the csvs, the first value is the class of that instance; the
rest are the attributes that we'll use for classification. (all instances
should have the same number of attributes)
"""

from __future__ import division
from collections import defaultdict
from functools import reduce
import math

class Instance(object):
    """An instance has a class (cl) and a list of attributes."""
    def __init__(self, cl, attributes):
        self.cl = cl
        self.attributes = attributes

## TODO: add the m-estimates, so we can get something like smoothing here too.
def estimate_probabilities(training):
    """Estimate the probability of each attribute value, given each class, from
    the training data. Returns a pair of dictionaries:
    
    The first is the "class probabilities", and goes from classes to
    probabilities.
    
    The second is the "attribute probabilities" and goes from
    (cl,attribute-pos,attribute-value) to probabilities."""

    attributecounts = defaultdict(lambda:0)
    classcounts = defaultdict(lambda:0)

    # x is an instance in the training set.
    for x in training:
        cl = x.cl
        classcounts[cl] += 1
        for (pos,xi) in zip(range(len(x.attributes)), x.attributes):
            attributecounts[(cl, pos, xi)] += 1

    classprobs = defaultdict(lambda:0)
    n = len(training)
    for cl in classcounts.keys():
        classprobs[cl] = classcounts[cl] / n

    ## probability of seeing that field assigned that value, given that it's a
    ## member of that class.
    attributeprobs = defaultdict(lambda:0)
    ## one key for each combination of class and attribute/value. Value is the
    for key in attributecounts.keys():
        attributeprobs[key] = attributecounts[key] / classcounts[key[0]]
    return classprobs, attributeprobs

def load_dataset(fn):
    with open(fn) as infile:
        lines = infile.readlines()
        out = []
        for line in lines:
            splitted = line.strip().split(",") 
            cl = splitted[0]
            attributes = splitted[1:]
            instance = Instance(cl, attributes)
            out.append(instance)
        return out

def product(nums):
    return reduce(lambda x,y: x*y, nums)

def classify(instance, class_probs, attribute_probs):
    """Return a classification for this instance, given the class probabilities
    and attribute probabilities."""

    possible_cls = list(class_probs.keys())

    attr_pairs = [(pos, value)
                  for (pos,value) in zip(range(len(instance.attributes)),
                                         instance.attributes)]

    scores = [(class_probs[cl] * 
               product([attribute_probs[(cl, pos, value)]
                        for (pos,value) in attr_pairs]) )
              for cl in possible_cls]
    maxindex = scores.index(max(scores))
    maxclass = possible_cls[maxindex]
    correct = (instance.cl == maxclass)
    print("%s %s %s" %
      (maxclass, ("Correct!" if correct else "Wrong!"), scores))

import sys
def main():
    if len(sys.argv) != 3:
        print("usage: %s training.csv test.csv" % (sys.argv[0],))

    training = load_dataset(sys.argv[1])
    classprobs,attributeprobs = estimate_probabilities(training)

    test = load_dataset(sys.argv[2])
    for instance in test:
        classify(instance, classprobs, attributeprobs)

if __name__ == "__main__": main()
