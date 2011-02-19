#!/usr/bin/env python3

"""
SAT solver with Dancing Links.
"""

from sparsematrix import SparseMatrix
from dlx import DLX

import sys
import subprocess
import os

def newrow(var, numvars, clauses, on):
    """Given the variable as an int, the number of variables, the list of
    clauses, and whether or not this is a row representing a 1 setting for this
    variable, generate a new row for the grid."""
    row = [0] * numvars
    row[var - 1] = 1

    needle = var if on else (- var)

    for clause in clauses:
        row.append(1 if needle in clause else 0)
    return row

def buildgrid(numvars, clauselines):
    """Given a number of variables and a list of lines that each represent
    clauses, return a list of lists where each entry is 0 or 1.

    The first numvars entries in each sublist are 1 iff that row represents an
    assignment 
    The rest numclauses entries are each 1 iff this assignment /causes/ the
    corresponding clause to be true. (there are numclauses clauses.)
    """
    rows = []

    clauses = [list(map(int,line.split())) for line in clauselines]

    for var in range(1, numvars+1):
        rows.append(newrow(var, numvars, clauses, True))
        rows.append(newrow(var, numvars, clauses, False))
    return rows

def getproblem(fn):
    """Given a filename, return a tuple containing the number of variables and
    a list with each of the clause lines."""

    with open(fn, "r") as infile:
        nvars = int(infile.readline())
        clauses = [line.strip() for line in infile]
        return (nvars, clauses)

def printresults(results):
    """Given the indices of the nodes, check whether they're even, indicating a
    True assignment, or odd, indicating a False."""
    if results is None:
        print("Not satisfiable")
        return

    rowindices = sorted([node.rowindex for node in results
                                       if node is not None])
    for rowindex in rowindices:
        print("T" if (rowindex % 2 == 0) else "F")

def main():
    if(len(sys.argv) != 2):
        print("usage: python3 solveitup.py <problem description>")
        return

    numvars, clauses = getproblem(sys.argv[1])
    grid = buildgrid(numvars, clauses)

    dlx = DLX(SparseMatrix(grid))
    results = dlx.search()
    printresults(results)

if __name__ == "__main__": main()
