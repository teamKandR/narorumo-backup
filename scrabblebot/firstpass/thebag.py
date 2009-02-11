#!/usr/bin/python

"""From Wikipedia: http://en.wikipedia.org/wiki/Scrabble_letter_distributions

language editions of Scrabble contain 100 letter tiles, in the following
distribution:

* 2 blank tiles (scoring 0 points)
* 1 point: E:12, A:9, I:9, O:8, N:6, R:6, T:6, L:4, S:4, U:4
* 2 points: D:4, G:3
* 3 points: B:2, C:2, M:2, P:2
* 4 points: F:2, H:2, V:2, W:2, Y:2
* 5 points: K:1
* 8 points: J:1, X:1
* 10 points: Q:1, Z:1"""

import random

bagoftiles = ['a']*9 \
  + ['b'] * 2  \
  + ['c'] * 2 \
  + ['d'] * 4 \
  + ['e'] * 12 \
  + ['f'] * 2 \
  + ['g'] * 3 \
  + ['h'] * 2 \
  + ['i'] * 9 \
  + ['j'] * 1 \
  + ['k'] * 1 \
  + ['l'] * 4 \
  + ['m'] * 2 \
  + ['n'] * 6 \
  + ['o'] * 8 \
  + ['p'] * 2 \
  + ['q'] * 1 \
  + ['r'] * 6 \
  + ['s'] * 4 \
  + ['t'] * 6 \
  + ['u'] * 4 \
  + ['v'] * 2 \
  + ['w'] * 2 \
  + ['x'] * 1 \
  + ['y'] * 2 \
  + ['z'] * 1 \
  + [' '] * 2

print bagoftiles
