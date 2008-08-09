from unittest import TestCase

from sudokudlx.sparsematrix import SparseMatrix
from copy import deepcopy

rows1 = [[0, 0, 1, 0, 1, 1, 0],
         [1, 0, 0, 1, 0, 0, 1],
         [0, 1, 1, 0, 0, 1, 0],
         [1, 0, 0, 1, 0, 0, 0],
         [0, 1, 0, 0, 0, 0, 1],
         [0, 0, 0, 1, 1, 0, 1]]

## Only columns 3 and 5 have 1s. This is an unsolvable cover problem.
rows2 = [[0, 0, 0, 0, 0, 1, 0],
         [0, 0, 0, 1, 0, 0, 0],
         [0, 0, 0, 0, 0, 1, 0],
         [0, 0, 0, 1, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 1, 0, 0, 0]]

matrix = SparseMatrix(rows1)
sparser = SparseMatrix(rows2)

class MatrixTests(TestCase):
  def testInitMatrix(self):
    ## every column has a 1 in it, and there's a special column header.
    assert len(matrix.columns) == 8
    assert len(matrix.node_table) == 16

    ## not every column has a 1.
    assert len(sparser.columns) == 3
    assert len(sparser.node_table) == 5

  def testRowIndices(self):
    assert matrix.rowindices() == [0, 1, 2, 3, 4, 5]

  def testColIndices(self):
    assert matrix.colindices() == [0, 1, 2, 3, 4, 5, 6]
    assert sparser.colindices() == [3, 5]

  def testColIndecesFor(self):
    assert matrix.colindices_for(0) == [2, 4, 5]
    assert matrix.colindices_for(5) == [3, 4, 6]

  def testRowIndecesFor(self):
    assert matrix.rowindices_for(0) == [1, 3]
    assert matrix.rowindices_for(5) == [0, 2]

  def testLinkNodesInRows(self):
    table = matrix.node_table

    # leftmost wraps around to leftmost
    assert table[(0,2)].left == table[(0,5)]

    # leftmost has a right
    assert table[(0,2)].right == table[(0,4)]

    # rightmost wraps around to left
    assert table[(0,5)].right == table[(0,2)]

    # left of my right is me!
    assert table[(0,5)].right.left == table[(0,5)]

  def testLinkNodesInColumns(self):
    table = matrix.node_table
    coltable = matrix.column_table

    assert table[(0,2)].up == coltable[2]
    assert coltable[2].down == table[(0,2)]

    assert table[(2,1)].down == table[(4,1)]
    assert table[(4,1)].up == table[(2,1)]
    assert table[(4,1)].down== coltable[1]

  def testLinkColumns(self):
    coltable = matrix.column_table
    col_header = matrix.column_header

    assert coltable[0].left == col_header
    assert col_header.right == coltable[0]

    assert coltable[1].right == coltable[2]


  def testColumnSizes(self):
    coltable = matrix.column_table

    assert coltable[1].size == 2
    assert coltable[6].size == 3

  def testCover(self):
    """See figure 3 in the Knuth paper."""
    mat = deepcopy(matrix)
    coltable = mat.column_table
    table = mat.node_table
    col_header = mat.column_header

    mat.cover(coltable[0])

    assert col_header.right == coltable[1]
    assert coltable[1].left == col_header

    assert coltable[3].down == table[(5,3)]
    assert table[(5,3)].up == coltable[3]

    assert coltable[6].down == table[(4,6)]

  def testUncover(self):
    """See figure 3 in the Knuth paper."""

    mat = deepcopy(matrix)

    coltable = mat.column_table
    table = mat.node_table
    col_header = mat.column_header

    mat.cover(coltable[0])
    assert col_header.right == coltable[1]

    mat.uncover(coltable[0])
    assert coltable[1].left == coltable[0]
    assert col_header.right == coltable[0]

    assert coltable[3].down == table[(1,3)]
    assert table[(5,3)].up == table[(3,3)]

    assert coltable[6].down == table[(1,6)]
