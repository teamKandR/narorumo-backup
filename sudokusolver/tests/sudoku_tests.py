from unittest import TestCase

from sudokudlx.sparsematrix import SparseMatrix
from sudokudlx.sudoku import SudokuPuzzle
from sudokudlx.fileutil import build_sudoku
from sudokudlx import sudoku

from copy import deepcopy

blank = sudoku.build_blank_board()

class SudokuTests(TestCase):

  def testRowColToBox(self):
    r,c = 0,0
    box = sudoku.row_col_to_box(r,c)
    assert box == 0

    r,c = 8,8
    box = sudoku.row_col_to_box(r,c)
    assert box == 8

    r,c = 4,4
    box = sudoku.row_col_to_box(r,c)
    assert box == 4

  def testDlxRow(self):
    dlxrow = sudoku.dlx_row(0,0,1)
    assert len(dlxrow) == 324
    assert sum(dlxrow) == 4

    assert dlxrow[0] == 1
    assert dlxrow[1] == 0

  def testDlxRowsForSquare(self):
    assert len(sudoku.dlx_rows_for_square(0,0,1)) == 1
    assert len(sudoku.dlx_rows_for_square(0,0,0)) == 9

  def testEncode(self):
    r,c = 0,0
    encoded = sudoku.encode(r,c)
    assert encoded[0] == 1
    assert encoded[1] == 0

  def testDecode(self):
    encoded = sudoku.encode(0,0)

    out_r,out_c = sudoku.decode(encoded)

    assert out_r == 0
    assert out_c == 0

  def testDlxRowToRcv(self):
    dlxrow = sudoku.dlx_row(0,0,1)
    r,c,v = sudoku.dlx_row_to_rcv(dlxrow)
    assert 0 == r
    assert 0 == c
    assert 1 == v

  def testSquaresToDlxRows(self):
    blank_dlxrows = sudoku.squares_to_dlx_rows(blank)
    assert len(blank_dlxrows) == 729

    filledsquares = [[1,2,3,4,5,6,7,8,9],
                     [1,2,3,4,5,6,7,8,9],
                     [1,2,3,4,5,6,7,8,9],
                     [1,2,3,4,5,6,7,8,9],
                     [1,2,3,4,5,6,7,8,9],
                     [1,2,3,4,5,6,7,8,9],
                     [1,2,3,4,5,6,7,8,9],
                     [1,2,3,4,5,6,7,8,9],
                     [1,2,3,4,5,6,7,8,9]]
    filleddlxrows = sudoku.squares_to_dlx_rows(filledsquares)
    assert len(filleddlxrows) == 81

  def testSudokuPuzzle(self):
    blankPuzzle = SudokuPuzzle(blank)

    soln = blankPuzzle.solve()

    assert soln
    assert len(soln) == 9
    assert len(soln[0]) == 9

  def testNearWorstCase(self):
    """Test a supposedly-hard problem for sudoku solvers"""

    puzzle = build_sudoku("testcases/near-worst-case")
    soln = puzzle.solve()
    assert soln

  def testFailureForUnsolveable(self):
    """Shouldn't return a true value for an unsolvable board."""
    puzzle = build_sudoku("testcases/fail")
    soln = puzzle.solve()
    assert not soln
