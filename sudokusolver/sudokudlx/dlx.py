#!/usr/bin/env python2.5

import sys

class DLX(object):
  def __init__(self, matrix):
    self.matrix = matrix
    self.solution = []
  
  def choose_column(self):
    lowestsize = sys.maxint
    here = self.first_column
    choice = None

    while True:
      if here.size < lowestsize:
        choice = here
        lowestsize = here.size

      here = here.right
      if here == self.first_column: break

    return choice

#   search(k):
  def search(self):
    """Find a list of rows (nodes in rows, at least) that satisfies the set
    cover problem. """

#     if R[h] = h, print the current solution (see below) and return.
#     "If we're all out of columns, win!"

#     Otherwise, choose a column and cover it.

#     For each r <- D[c], D[D[c]] ... while r != c
#     "search down through the rows, until you've looped around."

#       set O[k] <- r
#       "add this row to the partial solution"

#       for each j <- R[r], R[R[r]] ... while j != r

#         cover column C[j]
#         "remove this column -- we've got it handled for the partial soln".

#       search (k+1).
#       "Look for the next part of the solution."

#       set r <- O[k] and c <- C[r].
#       "we didn't win..."

#       for each j <- L[r], L[L[r]] ... while j != r.
#       "for every column in the row that was in our partial soln"

#         uncover column C[j] (see below)
#         "put that column back"

#     Uncover column c and return.
#     "Didn't win with column c, back out."

    pass
