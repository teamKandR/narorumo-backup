#!/usr/bin/env python

"""How hard is it to do the word-rearranging thing in a properly recursive
way? Will it blow up too big?"""

import string
import copy
import time

def print_timing(func):
  def wrapper(*arg):
    t1 = time.time()
    res = func(*arg)
    t2 = time.time()
    print '%s took %0.3f ms' % (func.func_name, (t2-t1)*1000.0)
    return res
  return wrapper

@print_timing
def my_permute(lst):
  return permute(lst)

def permute(lst):
  if(1 >= len(lst)):
    return [lst]

  out = []
  for firstindex in xrange(len(lst)):
    first = lst[firstindex]
    rest = lst[:firstindex] + lst[firstindex+1:]

    restpermutes = permute(rest)
    for restpermute in restpermutes:
      out.append( [first] + restpermute )

  return out


## from http://snippets.dzone.com/posts/show/3556
@print_timing
def their_permute(lst):
  return perm(lst)

def perm(l):
  sz = len(l)
  if sz <= 1:
    return [l]
  return [p[:i]+[l[0]]+p[i:] for i in xrange(sz) for p in perm(l[1:])]

def main():
  # print perm( list("food") )

  mine = my_permute(list("foodoofooo"))

  theirs = their_permute(list("foodoofooo"))

if "__main__" == __name__:
  main()
