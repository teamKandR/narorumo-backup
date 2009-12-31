#!/usr/bin/env python

"""
markov.py: Simple representation of a one-step Markov process, using matrix
multiplication from numpy.
"""

__author__ = "Alex Rudnick (alex.rudnick@gmail.com)"

import numpy


# Rows are the number of lines you're coming from, columns are the number of
# busy lines you're going to. Individual entries are probabilities.
P = numpy.matrix( [[0.2, 0.5, 0.2, 0.1],
                   [0.3, 0.4, 0.2, 0.1],
                   [0.1, 0.3, 0.4, 0.2],
                   [0.1, 0.1, 0.3, 0.5]])

# initial state.
init = numpy.matrix( [0.5, 0.3, 0.2, 0.0] )

def main():
    print "Initial state probabilities: ", init

    print "after one step: ", init * P
    print "two-step transition matrix (matches with slides)\n", P * P
    print "after four steps: ", (init * P * P * P * P)

if __name__ == "__main__": main()
