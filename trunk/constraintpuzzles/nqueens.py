#!/usr/bin/env python

"""
Solve the n-queens problem in a pretty naive way, with constraint programming.
"""

from __future__ import division
import constraint
import sys

def printreturn(val):
    print val
    return val

def addDiagonalConstraints(problem, columns):
    """Make it so that the row values for each pair of two column variables
    can't be on a diagonal -- ie, have a slope of 1 or -1 from one another."""

    for col1 in columns:
        for col2 in columns:
            if col1 == col2: continue

            problem.addConstraint(
                lambda r1, r2, c1=col1, c2=col2: abs(r1 - r2) != abs(c1 - c2),
                [col1, col2])

def buildProblem(N):
    problem = constraint.Problem()
    problem.addVariables(range(1,N+1), range(1,N+1))
    problem.addConstraint(constraint.AllDifferentConstraint())
    addDiagonalConstraints(problem, xrange(1,N+1))
    
    return problem

def solveN(N):
    problem = buildProblem(N)
    return problem.getSolutions()

def oneSolutionN(N):
    problem = buildProblem(N)
    return problem.getSolution()

def main():
    if len(sys.argv) != 2:
        print "Assuming N=8."
        N = 8
    else:
        N = int(sys.argv[1])
    solns = solveN(N)
    print solns

if __name__ == "__main__": main()
