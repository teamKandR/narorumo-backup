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

## Each 1 in the matrix has five fields, L, R, U, D, and C.

L,R,U,D,C = range(5)
