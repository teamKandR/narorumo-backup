#!/usr/bin/env python2.5

import copy
import sys

class Square:
  def __init__(self, board, rowindex, colindex):
    self.possibilities = set(range(1, 9 + 1))
    self.fixed = False
    self.answer = None
    self.board = board
    self.rowindex = rowindex
    self.colindex = colindex

  def set(self, val):
    print ("setting (%d,%d) to %d" % (self.rowindex, self.colindex, val))

    if (self.fixed and val != self.answer):
      raise Exception("answer was %d, tried to set to %d" % (self.answer, val))

    self.answer = val
    self.possibilities = set()

    self.fixed = True
    self.board.square_has_fixed(self)

  def remove_possibility(self, x):
    if self.fixed:
      return

    if x in self.possibilities:
      self.possibilities.remove(x)
      print ("removing from (%d,%d): %d"
          % (self.rowindex,self.colindex,x))

    if len(self.possibilities) == 1:
      self.set(self.possibilities.pop())
      print ("eliminated everything else: (%d,%d) is %d"
          % (self.rowindex,self.colindex,self.answer))

  def __repr__(self):
    if self.fixed:
      return str(self.answer)
    else:
      return "x"

  def __str__(self):
    return repr(self)

class Area(object):
  def infer(self, board):
    """Where the magic happens. If we have a fixed square in this area,
    remove that possibility from every other square in the area. If a square is
    out of possibilities, make it be fixed."""

    return False

  def contradiction(self):
    """For now -- if more than one square is fixed to the same value
    (pigeonhole principle: if the size of the set is smaller than the size of
    the list), then true. In the future, we may encode more rules and do
    better."""

    fixeds = [square.answer for square in self.squares if square.fixed]
    return len(set(fixeds)) < len(fixeds)

  def remove_possibility(self, val):
    for square in self.squares:
      square.remove_possibility(val)

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

class Row(Area):
  def __init__(self, board, rowindex, size=9):
    self.squares = []
    self.area_index = rowindex

    colindex = 0
    for i in range(size):
      self.squares.append(Square(board, rowindex, colindex))
      colindex += 1

class Column(Area):
  def __init__(self, board, squares, area_index, size=9):
    self.squares = squares
    self.area_index = area_index

class Box(Area):
  def __init__(self, board, squares, area_index, size=9):
    self.squares = squares
    self.area_index = area_index

class Board:
  def __init__(self):
    self.rows = []
    self.size = 9

    rowindex = 0
    for i in range(9):
      self.rows.append(Row(self, rowindex))
      rowindex += 1

    self.cols = self.buildCols()
    self.boxes = self.buildBoxes()

  def assume(self, row, col, val):
    newboard = copy.deepcopy(self)
    newboard.set(row, col, val)

    return newboard

  def set(self, row, col, val):
    """Fix the one square, then remove the possibility from everybody else in
    the apropos row, column, and box."""
    therow = self.rows[row]
    therow[col].set(val)

  def square_has_fixed(self, square):
    rowindex = square.rowindex 
    colindex = square.colindex 
    boxindex = row_col_to_box_index(rowindex,colindex)

    answer = square.answer

    therow = self.rows[rowindex]
    thecol = self.cols[colindex]
    thebox = self.boxes[boxindex]

    therow.remove_possibility(answer)
    thecol.remove_possibility(answer)
    thebox.remove_possibility(answer)

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

  def infer_areas(self, areas):
    """Pass in a list of Area objects, such as self.rows or self.boxes. If
    we've made at least one step forward, return true."""
    made_changes = False 

    for area in areas:
      made_changes |= area.infer(self)

    return made_changes

  def infer_rows(self):
    return self.infer_areas(self.rows)

  def infer_cols(self):
    return self.infer_areas(self.cols)

  def infer_boxes(self):
    return self.infer_areas(self.boxes)

  def solve(self):
    """Crunch until we think we're done. Return True when we find a solution,
       or False if we think we can't find one."""
    if self.contradiction():
      return False

    made_changes = True
    while made_changes:
      made_changes = False

      made_changes |= self.infer_rows()
      made_changes |= self.infer_cols()
      made_changes |= self.infer_boxes()

      made_changes |= self.infer_rows()
      made_changes |= self.infer_cols()
      made_changes |= self.infer_boxes()

    if self.solved():
      return True

    # make assumptions!

    # find next free square, assume about it, solve that. Failing that, try the
    # box after that one.

    return False

  def contradiction(self):
    for row in self.rows:
      if row.contradiction():
        return True
    for col in self.cols:
      if col.contradiction():
        return True
    for box in self.boxes:
      if box.contradiction():
        return True
    return False

  def solved(self):
    """Return True if we're done, False otherwise."""
    for row in self.rows:
      for i in range(9):
        if not row[i].fixed:
          return False
    return True

  def buildBox(self, i):
    """build the ith box and return it."""
    top = i - (i % 3)
    left = (i % 3) * 3

    squares = []
    for row in range(top, top+3):
      for col in range(left, left+3):
        squares.append(self.rows[row][col])
    
    out = Box(self, squares, i)
    return out

  def buildBoxes(self):
    out = map(self.buildBox, range(self.size))
    return out

  def buildCol(self, i):
    """build the ith column and return it."""
    squares = []
    for row in range(self.size):
      squares.append(self.rows[row][i])

    out = Column(self, squares, i)
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

def row_col_to_box_index(row, col):
  """... what does this do? """
  return (row - (row % 3)) + (col / 3)

def row_col_to_boxpos(row, col):
  """What does this do?"""
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
