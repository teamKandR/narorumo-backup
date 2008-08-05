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

  def search(self):
    """
    search(k):
      if R[h] = h, print the current solution (see below) and return.
      "If we're all out of columns, win!"

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

  def cover(self, column):
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

  def uncover(self, column):
    """
    Uncovering a column...

    for each i <- U[c], U[U[c]] ... while i != c
      for each j <- L[i], L[L[i]] ... while j != i
        increment S[C[j]]
        set U[D[j]] to j, D[U[j]] <- j

    set L[R[c]] <- c and R[L[c]] <- c
    """
    pass


  def choosecolumn(self):
    ### choosing a column object
    """To choose a column object,... (to minimize the branching factor), we set
    s to infinity and...

    for each j <- R[h], R[R[h]]... while j != h
      if S[j] < s, set c <- j and s <- S[j].

    Which is to say, just iteratively pick the column with the fewest 1s in it.
    """

nodes = {}
def build_node(row,col):
  nodes[(row,col)] = Node()

def get_node(row, col):
  if nodes.has_key((row,col)):
    return nodes[(row,col)]
  else:
    return None

## Each 1 in the matrix has five fields, left/right/up/down, column
class Node(object):
  def __init__(column):
    self.left = self
    self.right = self
    self.up = self
    self.down = self
    self.column = column

class Column(object):
  def __init__(self, index, col)
    print "Column constructor!"
    self.name = index
    self.size = 0

    self.column_head = None

    self.include_col(col, index)
    self.link_column(col)

    self.print_col()

  def print_col():
    pass

  def include_col(self, col, col_index):
    row_index = 0
    for here in col:
      if here:
        build_node(row_index, col_index)
        
  def link_column(self, col):
    prev = None
    top = None
    row_index = 0

    for place in col:
      if place:
        node = get_node(row_index, self.col_index)

        if not top:
          top = get_node(row_index, self.col_index)

        if prev:
          node.up = prev
          prev.down = node

      prev = node
      row_index += 1

    node.down = top
    top.up = node

  def add(self, otherCol):
    """Link in this column, to the right."""
    pass

def buildDLX(rows):
  """"Take a list of rows, where each row is a list of 0s and 1s, build a DLX
  object out of it."""

  ## what to do.

  ## build a Column object for each column, using asCols to get columns as
  ## lists. This will hook up the up, down, and header links for each one-Node.

  ## hook up the left and right for each column, at the top. Then go down each
  ## column and find the apropos lefts and rights, hook those up too.

  out = DLX()
  cols = asCols(rows)

  columns = [Column(i, col) for col,i in zip(range(len(rows[0])), cols) ]

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
