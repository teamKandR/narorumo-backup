#!/usr/bin/python

import sudoku
import sys

## main.
try:
    arg = sys.argv[1]
except (IndexError):
    arg = "board"

board = sudoku.read_board(arg)
done = sudoku.solve(board, 0)

sudoku.print_board(done)
