#!/usr/bin/env python

"""
fsa.py: Simple implementation of a finite-state automaton with matrix
multiplication, using numpy.
"""

__author__ = "Alex Rudnick (alex.rudnick@gmail.com)"

import numpy

# Rows are the state you're coming from, columns are the state you're going to.
# If there's a 1 in the slot (row,col) then the letter specified will send you
# from state #row to state #col.

# Transition matrix for the letter a.
a = numpy.matrix( [[0, 0, 0, 1],
                   [0, 0, 0, 1],
                   [0, 0, 1, 0],
                   [0, 0, 0, 1]])

b = numpy.matrix( [[0, 1, 0, 0],
                   [0, 1, 0, 0],
                   [0, 0, 0, 0],
                   [0, 1, 0, 0]])

c = numpy.matrix( [[0, 0, 1, 0],
                   [0, 0, 0, 0],
                   [0, 0, 0, 0],
                   [0, 0, 0, 0]] )

TRANSITIONS = {"a":a, "b":b, "c":c}

# initial state.
init = numpy.matrix( [1, 0, 0, 0] )

# final states
final = numpy.matrix( [1, 1, 1, 0] )
final = final.transpose()

def follow_path(chars, initial, transitions, final):
    """Given a string of characters, an initial state matrix, a dictionary
    mapping from characters to transition matrices, and a final-states matrix,
    do the specified state transitions and decide whether we're in an accept
    state."""

    print "\n" + ("-" * 80)
    print "taking this path:", chars

    current = initial
    print "Starting right here:", current

    for c in chars: 
        current = current * transitions[c]
        print "Now in these states:", current

    print "Accept? ", current * final

def main():
    follow_path("c", init, TRANSITIONS, final)
    follow_path("abbab", init, TRANSITIONS, final)
    follow_path("bababa", init, TRANSITIONS, final)

if __name__ == "__main__": main()
