#!/usr/bin/env python

"""
Given a filename (or optionally part of a filename), print the path where it
sits. If the path is the name of a directory, just print that.
"""

import sys
import os
from os.path import join, getsize

def strictmatch(needle, haystack):
  return needle == haystack

def loosematch(needle, haystack):
  return needle.lower() in haystack.lower()

def find(needle, matcher):
  """
  Start at the current directory and walk down from here until we find a file
  that matches (with the given match function) the specified needle. When we
  find it, print its location.
  """
  for root,dirs,files in os.walk("."):
    for dir in dirs:
      if matcher(needle,dir):
        path = join(root, dir)
        print path
        return

    for file in files:
      if matcher(needle,file):
        print root
        return
  print "."

def main():
  if len(sys.argv) < 2:
    print "."
    return

  matcher = strictmatch
  if len(sys.argv) > 2 and sys.argv[1] == "-l":
    matcher = loosematch

  needle = sys.argv[-1]
  find(needle, matcher)

if __name__ == '__main__':
  main()
