#!/usr/bin/env python2.5

from sys import argv
from sudokudlx.sudoku import SudokuPuzzle
from sudokudlx.fileutil import build_sudoku

def solve_fn(fn):
  puzzle = build_sudoku(fn)
  soln = puzzle.solve() 

  if soln:
    for row in soln:
      print row
  else:
    print "Fail!"

def main():
  for fn in argv[1:]:
    print ("*** %s ***" % fn)
    solve_fn(fn)

if __name__ == "__main__":
  main()
