#!/usr/bin/env python2.5

import sys

## Each 1 in the matrix has five fields, left/right/up/down, column

node_table = {}
SOLUTION = []

class DLX(object):
  def __init__(self, columns):
    self.columns = columns
    self.first_column = columns[0]

    self.current_column = self.first_column

    self.link_columns()
    self.link_nodes()

  def search(self, position):
    current_column = self.current_column
    if current_column.right == current_column:
      print SOLUTION
      return SOLUTION

    current_column = self.choose_column()
    print "I chose", current_column

    self.cover(current_column)

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

  def cover(self, column):
    ## Set L[R[c]] <- L[c] and R[L[c]] <- R[c]

    # go down until we've looped over, and ...
    ## go right until we've looped around, and...
    ### take out that node from the column (unlink it from its ups and downs)
    ### and decrement the SIZE on its column.
    pass

  def printout(self):
    print "dlx printout!!"
    first = self.first_column
    here = first

    while True:
      print "loop:", here
      here = here.right
      if here == first: break
    print "end dlx printout!!"

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
    # Find all of the rowindexes that we use, grab each node that lives at that
    # rowindex, and link them together in a loop.
    keys = node_table.keys()
    rowindices = [rowindex for (rowindex,colindex) in keys]
    rowindices = list(set(rowindices))

    rowindices.sort()

    for rowindex in rowindices:
      rowkeys = [(r,c) for (r,c) in keys if r == rowindex]
      rowkeys.sort(key=lambda(pair): pair[1])

      leftmost = None
      prev = None
      for key in rowkeys:
        node = node_table[key]

        if not leftmost:
          leftmost = node
        if prev:
          node.left = prev
          prev.right = node

        prev = node

      node.right = leftmost
      if leftmost:
        leftmost.left = node


class Node(object):
  def __init__(self, column, rowindex, colindex):
    self.left = self
    self.right = self
    self.up = self
    self.down = self
    self.column = column

    self.rowindex = rowindex
    self.colindex = colindex

    node_table[(rowindex,colindex)] = self

  def __repr__(self):
    return ("(%d,%d)" % (self.rowindex, self.colindex))

class Column(object):
  def __init__(self, ones, name):
    self.size = len(ones)
    self.top = None
    self.left = None
    self.right = None
    self.name = name

    self.ones = ones
    self.link_ones()

  def __repr__(self):
    return str(self)

  def __str__(self):
    return ("<column %d>" % self.name)

  def link_ones(self):
    prev = None
    top = None

    for one in self.ones:
      rowindex,colindex = one
      node = Node(self, rowindex, colindex)

      if not top:
        top = node
      elif prev:
        node.up = prev
        prev.down = node
      prev = node

    node.down = top
    if top:
      top.up = node

    self.top = top

  def printout(self):
    topnode = self.top
    here = topnode

    while True:
      print here
      here = here.down 
      if here == topnode: break

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

def sparseMatrix(rows):
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

  sparse = sparseMatrix(rows)
  columns = list_columns(sparse)
  dlx = DLX(columns)
  
  dlx.search(0)

if __name__ == "__main__":
  main()
