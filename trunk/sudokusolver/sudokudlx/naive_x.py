#!/usr/bin/env python2.5

# Naive approach to Algorithm X. Copy state as we go.

import sys
import copy

# If A is empty, the problem is solved; terminate successfully.
# Otherwise choose a column c (deterministically)
# Choose a row r, such that A[r, c] = 1 (nondeterministically)
# Include r in the partial solution
# For each j such that A[r,j] = 1
#   delete column j from matrix A
#   for each i such that A[i, j] = 1,
#     delete row i from matrix A
# Repeat this algorithm recursively on the reduced matrix A.

# (particularly, we want to choose the column with the fewest 1s in it.)


