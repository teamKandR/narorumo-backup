#!/usr/bin/env python

import math

# gravitational constant
G = 6.67428e-11

# mass of the earth
M = 6.0e24

def first_thrust(r1, r2):
  leftpart = math.sqrt((G * M) / r1)
  rightpart = math.sqrt((2 * r2) / (r1 + r2)) - 1
  deltav = leftpart * rightpart
  return deltav

def second_thrust(r1, r2):
  leftpart = math.sqrt((G * M) / r2)
  rightpart = 1 - math.sqrt((2 * r1) / (r1 + r2))
  out = leftpart * rightpart
  return out

def pythag(x, y):
  return math.sqrt((x ** 2) + (y ** 2))

def printDeltaVs(r1, r2, pos0, pos1):
  deltav = first_thrust(r1, r2)
  print "deltaV is", deltav

  deltavprime = second_thrust(r1, r2)
  print "deltaVprime is", deltavprime

  sx0,sy0 = pos0
  sx1,sy1 = pos1

  vx = sx1 - sx0
  vy = sy1 - sy0

  print "vx:", vx, "vy:", vy

  speed = pythag(vx, vy)
  print "speed:", speed

  deltavx = (vx / speed) * deltav
  deltavy = (vy / speed) * deltav

  print "deltavx:", deltavx, "deltavy:", deltavy

  deltavpx = (vx / speed) * deltavprime
  deltavpy = (vy / speed) * deltavprime
  print "deltavpx:", deltavpx, "deltavpy:", deltavpy

def main():
  print "HOHMANN 1001"
  r1 = 6557000.0
  r2 = 42164000.0
  sx0 = -6556995.342902722
  sy0 = 7814.93273851
  sx1 = -6556981.371617502
  sy1 = 15629.85437593658
  printDeltaVs(r1, r2, (sx0,sy0), (sx1, sy1))

  print "HOHMANN 1002"
  r1 = 8990155.616006106
  r2 = 21082000.0
  sx0 = -6352279.0
  sy0 = 6361717.0
  sx1 = -6347555.0
  sy1 = 6366431.0
  printDeltaVs(r1, r2, (sx0,sy0), (sx1, sy1))

  print "HOHMANN 1003"
  r1 = 8357000.000000492
  r2 = 28109333.333333332
  sx0 = -8356997.133018618
  sy0 = 6922.33535852
  sx1 = -8356988.53207644
  sy1 = 13844.665967439283
  printDeltaVs(r1, r2, (sx0,sy0), (sx1, sy1))

  print "HOHMANN 1004"
  # these are the first two steps of 1004.
  sx0 = 7875.21543324
  sy0 = -6456995.197536153
  sx1 = 15750.419151926055
  sy1 = -6456980.790151756
  r1 = 6457000.0
  r2 = 38330909.0
  printDeltaVs(r1, r2, (sx0,sy0), (sx1, sy1))

if __name__ == "__main__": main()
