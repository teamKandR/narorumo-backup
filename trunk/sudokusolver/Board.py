#!/usr/bin/env python2.5

import copy
import sys

class Area:
  def __init__(self, size=9, lst=None):
    if(lst):
      self.squares = copy.deepcopy(lst)
    else:
      self.squares = []
      for i in range(size):
        self.squares.append( range(1,size+1) )

  def __getitem__(self, key):
    return self.squares[key]

  def __setitem__(self, key, item):
    self.squares[key] = item

  def __repr__(self):
    return str(self.squares)

  def __str__(self):
    out = ""
    for square in self.squares:
      if isinstance(square, list):
        out += "? "
      else:
        out += (str(square) + " ")
    return out

class Board:
  def __init__(self):
    self.rows = []
    self.size = 9

    for i in range(9):
      self.rows.append(Area())

    self.cols = self.buildCols()
    self.boxes = self.buildBoxes()

  def assume(self, row, col, val):
    newboard = copy.deepcopy(self)
    newboard.set(row, col, val)

    return newboard

  def set(self, row, col, val):
    therow = self.rows[row]
    thecol = self.cols[col]
    thebox = self.boxes[row_col_to_box(row,col)]

    # set in rows.
    therow[col] = val

    # set in cols.
    thecol[row] = val

    # set in boxes.
    boxpos = row_col_to_boxpos(row, col)
    thebox[boxpos] = val

  def as_list(self):
    out = []
    rowindex = 0
    for row in self.rows:
      out.append([])
      for square in row.squares:
        if isinstance(square, list):
          out[rowindex].append('?')
        else:
          out[rowindex].append(square)
      rowindex += 1

    return out
    

  def solve(self):
    """Crunch until we think we're done. Return True when we find a solution,
       or False if we think we can't find one."""
    if(self.contradiction()):
      return False

    return True

  def contradiction(self):
    return False

  def buildBox(self, i):
    """build the ith box and return it."""
    top = i / 3
    left = i % 2

    squares = []
    for row in range(top, top+3):
      for col in range(left, left+3):
        squares.append(self.rows[row][col])
    
    out = Area(9, squares)
    return out

  def buildBoxes(self):
    out = map(self.buildBox, range(self.size))
    return out

  def buildCol(self, i):
    """build the ith column and return it."""
    squares = []
    for row in range(self.size):
      squares.append(self.rows[row][i])

    out = Area(9, squares)
    return out

  def buildCols(self):
    out = map(self.buildCol, range(self.size))
    return out

  def __str__(self):
    out = ""
    for row in self.rows:
      out += str(row)
      out += "\n"
    return out

def row_col_to_box(row, col):
  return (row - (row % 3)) + (col / 3)

def row_col_to_boxpos(row, col):
  return ((row % 3) * 3) + (col % 3)

def board_from_stream(bf):
  lines = bf.readlines()
  board = Board()
  
  row = 0
  for line in lines:
    col = 0
    cells = line.split()
    for cell in cells:
      cell = int(cell)
      if(cell):
        board.set(row, col, cell)

      col += 1
    row += 1
  return board

def read_board(fn):
  try:
    boardfile = open(fn, 'r')
    board = board_from_stream(boardfile)
  except(IOError):
    print "File not found!"
    board = Board()
  return board

def set_constraints(board, constraints):
  """constraints is of the form [ (row,col,val) ...]"""
  for constraint in constraints:
    row,col,val = constraint
    board.set(row, col, val)

def main():
  try:
    arg = sys.argv[1]
  except (IndexError):
    arg = "board"

  board = read_board(arg)
  print board

if __name__ == "__main__":
  main()
