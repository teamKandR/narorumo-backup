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

def find(needle, loose=False):
  match = strictmatch
  if loose:
    match = loosematch

  for root,dirs,files in os.walk("."):
    for dir in dirs:
      if match(needle,dir):
        path = join(root, dir)
        print path
        return

    for file in files:
      if match(needle,file):
        print root
        return

  print "."

def main():
  loose = False
  if len(sys.argv) > 2 and sys.argv[1] == "-l":
    loose = True
  needle = sys.argv[-1]
  find(needle, loose)

if __name__ == '__main__':
  main()
