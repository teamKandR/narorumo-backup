#!/usr/bin/env python2.5 

from boardstructure import *
from wordsdb import WordsDB

db = WordsDB()

def possible_moves(board, rack):
  """Rather more sophisticated. Takes the letters in your rack, and given
  the whole state of the board, finds out what words you could make. For
  serious this time."""

  exposed = board.exposed_letters()
  out = []

  for square in exposed:
    x,y = square[1:]
    withrack = rack + square[0]

    candidatewords = db.possiblewords(withrack)

    for word in candidatewords:
      m_right = Move(word, x, y, RIGHT)
      m_down = Move(word, x, y, DOWN)

      created_right = board.can_perform(m_right)
      created_down = board.can_perform(m_down)
      if created_right:
        out.append((m_right, created_right))
      if created_down:
        out.append((m_down, created_down))

  return out


def conceivable_moves(board, rack):
  """This is all the words that you could maybe build, given your rack and
  the exposed letters on the board (taken one at a time). Other than which
  ones are 'exposed', spatial information not taken into account. We'll have
  to filter for that somewhere else."""

  exposed = board.exposed_letters()
  out = []

  for letter in exposed:
    withrack = rack + letter[0]

    out = db.possiblewords(withrack)
  return out

def main():
  b = Board()
  m = Move("equine", 7, 7, RIGHT)
  m3 = Move("equinox", 12, 7, DOWN)

  b.perform(m)
  b.perform(m3)

  rack = "vidence"

  # print conceivable_moves(b, rack)
  moves = possible_moves(b, rack)
  for move in moves:
    print move

if "__main__" == __name__:
  main()
