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

dlx = DLX(rows1)

class DlxTests(TestCase):
  def testSearch(self):
    assert True

  def testChooseColumn(self):
    assert True
