#!/usr/bin/env python2.5

from sys import argv
from sudokudlx.sudoku import SudokuPuzzle

def build_sudoku(fn):
  infile = open(fn)
  lines = infile.readlines()
  infile.close()

  board = []

  for line in lines:
    line.strip()
    digits = line.split()

    row = []
    for digit in digits:
      row.append( int(digit) )
    board.append(row)

  puzzle = SudokuPuzzle(board)
  return puzzle
