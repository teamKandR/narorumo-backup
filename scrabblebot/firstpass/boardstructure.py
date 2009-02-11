#!/usr/bin/env python2.5 

from copy import deepcopy
from copy import copy
import wordsdb

"""How we gon' do?"""

DOWN, RIGHT = range(2)
BOARDSIZE = 15


class Board:
  def __init__(self):
    self.db = wordsdb.WordsDB()
    self.moves = []
    self.grid = []

    for row in xrange(BOARDSIZE):
      self.grid.append(
        [copy('-') for i in xrange(BOARDSIZE)])

  def find_words(self,row):
    out = []
    joined = "".join(row)
    splitted = joined.split('-')

    # print self.db
    print "food is a word?", self.db.isaword("food")

    for item in splitted:
      if len(item) > 1 and self.db.isaword(item):
        out.append(item)
      
    return out

  def all_words(self):
    horizwords = []
    vertwords = []

    horizwords = self.horiz_words()
    vertwords = self.vert_words()

    return horizwords + vertwords

  def vert_words(self):
    cols = self.columns()
    out = []

    for col in cols:
      words = self.find_words(col)
      out += words

    return out

    # words = map 

  def horiz_words(self):
    out = []
    for row in self.grid:
      words = self.find_words(row)
      out += words

    return out

  def columns(self):
    columns = []

    for x in xrange(BOARDSIZE):
      column = []
      for y in xrange(BOARDSIZE):
        column.append( self.grid[y][x] )

      columns.append(column)
    return columns

  def can_perform(self, move):
    fits = self.move_fits(move)

    if not fits:
      return False

    created = self.new_words(move)

    allwordsok = True
    for word in created:
      if not self.db.isaword(word):
        allwordsok = False
        break

    if fits and allwordsok:
      return created
    else:
      return False

  def move_fits(self, move):
    """Checks only whether a given word fits."""
    y = move.y
    x = move.x

    for letter in move.word:
      if x >= BOARDSIZE or y >= BOARDSIZE:
        return False

      if self.grid[y][x] != '-' and self.grid[y][x] != letter:
        return False

      if RIGHT == move.dir:
        x += 1
      else:
        y += 1
    return True

  def new_words(self, move):
    """Calculate all the words that would show up if you performed a move on a
    board. Do this by building the new board, finding all the words on that
    one, and then taking out all the words on the current board."""
    newboard = deepcopy(self)
    newboard.perform(move)

    newwords = newboard.all_words()
    oldwords = self.all_words()

    listdiff = [word for word in newwords if word not in oldwords]
    return listdiff

  def perform(self, move):
    self.moves.append(move)

    y = move.y
    x = move.x

    for letter in move.word:
      # print "letter, x, y:", letter, x, y
      self.grid[y][x] = letter
      if RIGHT == move.dir:
        x += 1
      else:
        y += 1

  def __str__(self):
    out = ""
    for row in self.grid:
      for square in row:
        out += "%s " % str(square)
      out += "\n"
    return out

  def exposed(self, i, j):
    leftrights = [-1, 0, 1]
    updowns = [-1, 0, 1]

    for leftright in leftrights:
      for updown in updowns:
        x = i + leftright
        y = j + updown
        if 0 < x < BOARDSIZE and 0 < y < BOARDSIZE:
          if self.grid[x][y] == '-':
            return True
    return False

  def exposed_letters(self):
    """All the letters on the board with whitespace around them in some
    sense. Returns tuples of the form (letter, x, y)"""
    out = []

    for i in xrange(BOARDSIZE):
      for j in xrange(BOARDSIZE):
        if ( '-' != self.grid[i][j] and self.exposed(i,j)):
          square = ( copy(self.grid[i][j]), j, i)
          out.append(square)
    return out

class Move:
  def __init__(self, word, x, y, dir):
    self.x = x
    self.y = y
    self.dir = dir
    self.word = word

  def __repr__(self):
    if(self.dir == RIGHT): dirstr = "right"
    else: dirstr = "down"
    return "move: %s @ (%d,%d), %s" % (self.word, self.x, self.y, dirstr)

def main():
  b = Board()
  m = Move("equine", 7, 7, RIGHT)

  b.perform(m)

  m2 = Move("nox", 12, 7, DOWN)
  m3 = Move("equinox", 12, 7, DOWN)

  print "new words:", b.new_words(m3)

  b.perform(m3)

  m4 = Move("nods", 12, 7, DOWN)

  print b
  print b.exposed_letters()

if __name__ == "__main__":
  main()
