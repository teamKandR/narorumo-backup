#!/usr/bin/env python2.5

import sys

class DLX(object):
  def __init__(self, matrix):
    self.matrix = matrix
    self.solution = [None] * (len(self.matrix.columns) - 1)
    self.header = self.matrix.column_header
  
  def choose_column(self):
    """Pick the most-constrained column still linked in the matrix."""
    lowestsize = sys.maxint
    header = self.header

    here = header.right
    choice = None

    while here != header:
      if here.size < lowestsize:
        choice = here
        lowestsize = here.size
      here = here.right

    return choice

  def search(self, k=0):
    """Find a list of rows (nodes in rows, at least) that satisfies the set
    cover problem. """

    if self.header.right == self.header:
      return self.solution

    tocover = self.choose_column()
    self.matrix.cover(tocover)

    rowstart = tocover.down
    while rowstart != tocover:
      self.solution[k] = rowstart

      here = rowstart.right
      while here != rowstart:
        self.matrix.cover(here.column)
        here = here.right

      further = self.search(k+1)
      if further: return further

      ## Didn't win, backing up.
      rowstart = self.solution[k]
      column = here.column

      here = rowstart.left
      while here != rowstart:
        self.matrix.uncover(here.column)
        here = here.left

      rowstart = rowstart.down

    self.matrix.uncover(tocover)
    return None
