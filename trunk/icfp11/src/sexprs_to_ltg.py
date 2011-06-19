#!/usr/bin/env python

from __future__ import print_function

## NB, this one's in python 2 and requires nltk.
import nltk

def print_ltg_style(tr):
    """print out the tree with the parens after the function name."""
    if isinstance(tr, str):
        print(tr, end="")
        return

    print(tr.node, end="")
    print("(",end="")
    for child in tr:
        print_ltg_style(child)
    print(")", end="")

def main():
    sexpr = ""
    try:
        while True:
            sexpr += raw_input()
    except EOFError:
        pass

    tr = nltk.tree.Tree(sexpr)
    print_ltg_style(tr)
    print()

if __name__ == "__main__": main()
