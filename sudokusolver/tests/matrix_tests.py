from unittest import TestCase

from sudokudlx.sparsematrix import SparseMatrix

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
    ## every column has a 1 in it.
    assert len(matrix.columns) == 7
    assert len(matrix.node_table) == 16

    ## not every column has a 1.
    assert len(sparser.columns) == 2
    assert len(sparser.node_table) == 5

  # def testLinks(self):
  def testRowIndices(self):
    assert matrix.rowindices() == [0, 1, 2, 3, 4, 5]

  def testColIndices(self):
    assert matrix.colindices() == [0, 1, 2, 3, 4, 5, 6]
    assert sparser.colindices() == [3, 5]

  def testColIndecesFor(self):
    assert matrix.colindices_for(0) == [2, 4, 5]
    assert matrix.colindices_for(5) == [3, 4, 6]

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
