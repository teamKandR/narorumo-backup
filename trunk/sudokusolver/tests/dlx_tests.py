from unittest import TestCase

from sudokudlx.sparsematrix import SparseMatrix
from sudokudlx.dlx import DLX
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

rows3 = [[1, 0],
         [0, 1]]

rows4 = [[1, 0, 0, 0],
         [0, 1, 0, 0],
         [0, 0, 1, 0],
         [0, 0, 0, 1]]

class DlxTests(TestCase):
  def testSearch(self):
    matrix = SparseMatrix(rows1)
    dlx = DLX(matrix)
    result = dlx.search()
    assert result

    mat2 = SparseMatrix(rows2)
    dlx2 = DLX(mat2)
    result2 = dlx2.search()
    assert not result2

    mat3 = SparseMatrix(rows3)
    dlx3 = DLX(mat3)
    res3 = dlx3.search()
    assert res3

    mat4 = SparseMatrix(rows4)
    dlx4 = DLX(mat4)
    res4 = dlx4.search()
    assert res4

  def testChooseColumn(self):
    matrix = SparseMatrix(rows1)
    dlx = DLX(matrix)
    coltable = matrix.column_table

    column = dlx.choose_column()

    assert coltable[0] == column

    matrix.cover(column) 

    # See Figure 3; column D now only has one node in it.
    after_cover = dlx.choose_column()
    assert coltable[3] == after_cover
