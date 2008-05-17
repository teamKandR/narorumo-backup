#!/usr/bin/env python2.5

"""Let's do this. As fast as we can (prefer native data structures to classes,
just well-documented) implement Knuth's DLX.

Knuth formulates the Exact Cover Problem thus:
"One way to illustrate the power of dancing links is to consider a general
problem that can be described abstractly as follows: Given a matrix of 0s and
1s, does it have a set of rows containing exactly one 1 in each column?"

Example matrix:

0 0 1 0 1 1 0 *
1 0 0 1 0 0 1
0 1 1 0 0 1 0
1 0 0 1 0 0 0 *
0 1 0 0 0 0 1 *
0 0 0 1 1 0 1

Answer: Just the starred rows.
0 0 1 0 1 1 0 *
1 0 0 1 0 0 0 *
0 1 0 0 0 0 1 *
"""

class DLX(object):
  def __init__(self):
    self.solution = []
    self.solutionIndex = 0
    self.rowCount = 0
    self.startingCount = 0
    self.here = None

    self.firstColumn = None

  def search():
    """
    search(k):
      if R[h] = h, print the current solution (see below) and return.
      "If we're all out of rows, win!"

      Otherwise, choose a column object c (see below)
      Cover column c.
      "Because we're in a column at all, we have a 1 here."

      For each r <- D[c], D[D[c]] ... while r != c
      "search down through the rows, until you've looped around."

        set O[k] <- r
        "add this row to the partial solution"

        for each j <- R[r], R[R[r]] ... while j != r
        "keep going right 'til you've looped around"

          cover column C[j]
          "remove this column -- we've got it handled for the partial soln".

        search (k+1).
        "Look for the next part of the solution."

        set r <- O[k] and c <- C[r].
        "we didn't win..."

        for each j <- L[r], L[L[r]] ... while j != r.
        "for every column in the row that was in our partial soln"

          uncover column C[j] (see below)
          "put that column back"

      Uncover column c and return.
      "Didn't win with column c, back out."
    """
    pass

  def cover(column):
    """
    Covering a column...

    cover(c):

      Set L[R[c]] <- L[c] and R[L[c]] <- R[c]

      for each i <- D[c], D[D[c]] ... while i != c:
        for each j <- R[i], R[R[i]] ... while j != i,
          set U[D[j]] <- U[j], D[U[j]] <- D[j]
          and decrement S[C[j]]
    """
    pass

  def uncover(column):
    """
    Uncovering a column...

    for each i <- U[c], U[U[c]] ... while i != c
      for each j <- L[i], L[L[i]] ... while j != i
        increment S[C[j]]
        set U[D[j]] to j, D[U[j]] <- j

    set L[R[c]] <- c and R[L[c]] <- c
    """
    pass


  def choosecolumn():
    ### choosing a column object
    """To choose a column object,... (to minimize the branching factor), we set s
    to infinity and...

    for each j <- R[h], R[R[h]]... while j != h
      if S[j] < s, set c <- j and s <- S[j].

    Which is to say, just iteratively pick the column with the fewest 1s in it.
    """

## Each 1 in the matrix has five fields, L, R, U, D, and C.
class Node(object):
  def __init__(self, rowindex, colindex):
    self.left = self
    self.right = self
    self.up = self
    self.down = self
    self.column = self

    self.rowindex = rowindex
    self.colindex = colindex

class Column(object):
  def __init__(self, name, col):
    print "Column constructor!"
    self.name = name
    self.size = 0

    self.left = self
    self.right = self
    self.up = self
    self.down = self
    self.column = self

    self.include_col(col)

  def include_col(self, col):
    for position in col:
      if position:
        node = Node(0,0)
        
        ## link it in.

  def add(self, otherCol):
    """Link in this column, to the right."""
    pass

def buildDLX(rows):
  """"Take a list of rows, where each row is a list of 0s and 1s, build a DLX
  object out of it."""

  out = DLX()
  cols = asCols(rows)

  columns = [Column(i, col) for col,i in zip(cols, range(len(rows[0]))) ]

  return out

def asCols(rows):
  """Transpose some rows."""
  nCols = len(rows[0])

  cols = [ [row[i] for row in rows] for i in xrange(nCols)]

  return cols

def main():
  rows = [[0, 0, 1, 0, 1, 1, 0],
          [1, 0, 0, 1, 0, 0, 1],
          [0, 1, 1, 0, 0, 1, 0],
          [1, 0, 0, 1, 0, 0, 0],
          [0, 1, 0, 0, 0, 0, 1],
          [0, 0, 0, 1, 1, 0, 1]]

  dlx = buildDLX(rows)
  print dlx

if __name__ == "__main__":
  main()
