from sudokudlx.dlx import DLX
from sudokudlx.sparsematrix import SparseMatrix

def build_blank_puzzle():
  return [[0] * 9,
          [0] * 9,
          [0] * 9,
          [0] * 9,
          [0] * 9,
          [0] * 9,
          [0] * 9,
          [0] * 9,
          [0] * 9]

def squares_to_dlx_rows(squares):
  """Take a sudoku puzzle (as a 9x9 nested list) and return a
  list-of-lists representing the 729-tall, 324-wide matrix."""

  out = []
  for row in range(0,9):
    for col in range(0,9):
      out += dlx_rows_for_square(row, col, squares[row][col])

  return out

def dlx_rows_for_square(rowindex, colindex, value):
  """Construct the dlx rows that correspond to a given square. If value is in
  the range [1, 9], we only return one row. Otherwise, we'll have many rows."""
  out = []

  if value in xrange(1,10):
    return [ dlx_row(rowindex, colindex, value) ]
  else:
    return map( (lambda(val): dlx_row(rowindex, colindex, val)), range(1,10))

def dlx_row(row, col, val):
  """Build the row representing the idea that a given sudoku row/col has a
  particular value. Will have length 324.
  Constraints go in the order:

  row-column: 81 columns that have 0s, save a 1 for the row,col that this
  possibility represents, at row*9 + col

  row-number: Each row only contains each number once. This section represents
  the row that this number is in, and the value; it will be 81 elements
  long, and contain a single 1 in the position row*9 + (value-1).

  column-number: Each column only contains each number once. This section
  represents the column  that this number is in, and the value. The 1 is in
  position column*9 + (value-1).

  box-number. Each box only contains each number once. The 1 in this section is
  at position boxnum*9 + (value-1)

  For example, the idea that row/col (0,0) has #1 looks like:
  row0col0 row0value1 col0value1 box0value1.

  Likewise, the possibility of #9 in (8,8), the lower-rightmost square, is:
  row8col8 row8value9 col8value9 box8value9.
  """
  box = row_col_to_box(row, col)
  val -= 1

  return encode(row,col) + encode(row,val) + encode(col,val) + encode(box,val) 

def dlx_row_to_rcv(dlxrow):
  """Pull (row,col,val) out from an encoded DLX list."""
  rowcol = dlxrow[0:81]
  rownum = dlxrow[81: 2*81]

  row,col = decode(rowcol)
  ignore,num = decode(rownum)

  return (row,col,num + 1)

def encode(major, minor):
  """Build a list of 81 values, with a 1 in the spot corresponding to the value
  of the major attribute and minor attribute."""
  out = [0] * 81
  out [major*9 + minor] = 1
  return out

def decode(lst):
  """Take a list of 81 values with a single 1, decode two values out of its
  position. Return them in a tuple (major,minor)."""

  position = lst.index(1)
  minor = position % 9
  major = position / 9
  return (major,minor)

def row_col_to_box(row, col):
  """Return the index for the box that the given (r, c) sudoku coordinates fits
  into. Boxes go like this:
  0 1 2
  3 4 5
  6 7 8
  """
  return (row - (row % 3)) + (col / 3)

class SudokuPuzzle(object):
  """Representation of a Sudoku puzzle, with methods to convert back and forth
  from an instance of DLX."""

  def __init__(self, squares):
    """Squares is a nested list representing an initial sudoku board. A 0
    represents an empty square, while a nonzero digit represents a square
    filled with that value."""

    self.dlxrows = squares_to_dlx_rows(squares)
    sparseMatrix = SparseMatrix(self.dlxrows)

    self.dlx = DLX(sparseMatrix)

  def solve(self):
    """Solve the sudoku puzzle. Return value is a nested list in the same
    format as the input to the constructor."""
    self.dlx_soln = self.dlx.search()
    
    if self.dlx_soln:
      self.soln_rows = [node.rowindex for node in self.dlx_soln if node]
    else:
      return None

    dlx_encoded_soln = [self.dlxrows[row] for row in self.soln_rows]
    rcvs = map(dlx_row_to_rcv, dlx_encoded_soln)

    out = build_blank_puzzle()

    for row,col,val in rcvs:
      out[row][col] = val

    return out
