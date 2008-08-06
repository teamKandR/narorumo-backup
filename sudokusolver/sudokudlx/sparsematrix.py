#!/usr/bin/env python2.5

import sys

## Each 1 in the matrix has five fields, left/right/up/down, column

## Just in case we need to look up at a particular (i,j) coordinate. Keys are
## tuples of the form (row, column).

class SparseMatrix(object):
  """The matrix from which we'll be picking out columns to solve the set-cover
  problem, for DLX."""

  def __init__(self, rows):
    """Takes a list of rows, each of which is a list of 0s and 1s."""

    self.node_table = {}

    cols = []
    for r in xrange(len(rows)):
      for c in xrange(len(rows[0])):
        if rows[r][c]:
          one = Node(r, c)
          self.node_table[(r,c)] = one

    self.build_columns()
    self.link_columns()
    self.link_nodes()

  def build_columns(self):
    """Put all the columns that this matrix has into self.columns"""
    keys = self.node_table.keys()
    with_repeats = [colindex for (rowindex,colindex) in keys]
    colindices = list(set(with_repeats))

    colindices.sort()

    self.columns = map( lambda(index): Column("col %d" % index) , colindices)

  def link_columns(self):
    prev = None
    first = None

    for column in self.columns:
      if not first:
        first = column
      elif prev:
        column.left = prev
        prev.right = column
      prev = column

    column.right = first
    if first:
      first.left = column

  def link_nodes(self):
    """Link all the nodes in the matrix together."""

    self.link_nodes_in_rows()
    # self.link_nodes_in_columns()

  def link_nodes_in_rows(self):
    """For each row, make the circular linked-list of those nodes."""
    rowindices = self.rowindices()

    for r in rowindices:
      colindices = self.colindices_for(r)

      prev = None
      first = None
      for c in colindices:
        node = self.node_table[(r,c)]

        if not first:
          first = node
        elif prev:
          node.left = prev
          prev.right = node
        prev = node

      node.right = first
      if first:
        first.left = node

  def link_nodes_in_columns(self):
    """For each column, make the circular linked-list of those nodes, with
    column header objects in the loop."""
    colindices = self.colindices()

    for r in rowindices:
      colindices = self.colindices_for(r)

      prev = None
      first = None
      for c in colindices:
        node = self.node_table[(r,c)]

        if not first:
          first = node
        elif prev:
          node.left = prev
          prev.right = node
        prev = node

      node.right = first
      if first:
        first.left = node

  def colindices(self):
    keys = self.node_table.keys()
    
    colindices = [colindex for (rowindex,colindex) in keys]
    colindices = list(set(colindices))
    colindices.sort()

    return colindices

  def rowindices(self):
    keys = self.node_table.keys()
    
    rowindices = [rowindex for (rowindex,colindex) in keys]
    rowindices = list(set(rowindices))
    rowindices.sort()

    return rowindices

  def colindices_for(self, r):
    """Take a given row index and return the list of the columns with nodes on
    that row."""

    keys = self.node_table.keys()
    
    colindices = [colindex for (rowindex,colindex) in keys if rowindex == r]
    colindices = list(set(colindices))
    colindices.sort()

    return colindices

class Node(object):
  def __init__(self, rowindex, colindex):
    self.left = self
    self.right = self
    self.up = self
    self.down = self

    self.rowindex = rowindex
    self.colindex = colindex

  def __repr__(self):
    return ("(%d,%d)" % (self.rowindex, self.colindex))

class Column(object):
  def __init__(self, name):
    self.size = 0
    self.top = None
    self.left = None
    self.right = None
    self.name = name

  def __repr__(self):
    return str(self)

  def __str__(self):
    return self.name

def list_columns(indices):
  """Take in a list of (row,col) pairs and produce a list of columns."""
  out = []

  cols = [col for (row, col) in indices]
  uniq_cols = list(set(cols))
  uniq_cols.sort()

  name = 0
  for colindex in uniq_cols:
    indices_in_column = [(row,col) for (row,col) in indices if col == colindex]

    next_column = Column(indices_in_column, name)
    out.append(next_column)
    name += 1
  
  return out

def rows_to_index_pairs(rows):
  """Take in rows, produce a list of (row,col) indices where there is a 1."""
  ones = []

  rowindex = 0
  for row in rows:
    colindex = 0
    for place in row:
      if place:
        ones.append((rowindex, colindex))

      colindex += 1
    rowindex += 1

  return ones
        
def transpose(rows):
  """Transpose some rows. Take in rows, return the cols of those rows"""
  nCols = len(rows[0])

  cols = [ [row[i] for row in rows] for i in xrange(nCols)]

  return cols

def main():
  rows = [[0, 0, 1, 0, 1, 1, 0],
          [1, 0, 0, 1, 0, 0, 1],
          [0, 1, 1, 0, 0, 1, 0],
          [1, 0, 0, 1, 0, 0, 0],
          [0, 1, 0, 0, 0, 0, 1],
          [0, 0, 0, 1, 1, 0, 1]]

  sparse = rows_to_index_pairs(rows)
  columns = list_columns(sparse)
  matrix = SparseMatrix(columns)

  matrix.printout()

if __name__ == "__main__":
  main()
