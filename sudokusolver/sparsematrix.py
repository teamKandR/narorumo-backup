#!/usr/bin/env python2.5

## Each 1 in the matrix has five fields, left/right/up/down, column

node_table = {}

class DLX(object):
  def __init__(self, columns):
    self.columns = columns
    self.first_column = columns[0]

    self.link_columns()
    pass

  def link_columns(self):
    for column in self.columns:
      top = column.top


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
  def __init__(self, ones):
    self.top = None
    self.ones = ones
    self.link_ones()

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
      if here == topnode:
        break

  def __repr__(self):
    return str(self.ones)

def list_columns(indices):
  """Take in a list of (row,col) pairs and produce a list of columns."""
  out = []

  cols = [col for (row, col) in indices]
  uniq_cols = list(set(cols))
  uniq_cols.sort()

  for colindex in uniq_cols:
    indices_in_column = [(row,col) for (row,col) in indices if col == colindex]
    out.append(Column(indices_in_column))
  
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
  
  print columns

  first = columns[0]
  first.printout()

if __name__ == "__main__":
  main()
