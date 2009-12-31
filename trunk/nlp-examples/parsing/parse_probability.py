#!/usr/bin/env python

"""
Find the probability for a parse tree, given a particular grammar.

Depends on nltk (developed with version 2.05b)

Alex Rudnick (alex.rudnick@gmail.com)
"""

from nltk.tree import *
import copy
import sys
import string

def words_to_indexes(tree):
    """Return a new tree based on the original tree, such that the leaf values
    are replaced by their indexs."""

    out = copy.deepcopy(tree)

    leaves = out.leaves()
    for index in range(0, len(leaves)):
        path = out.leaf_treeposition(index)

        out[path] = index + 1
    return out

def production(tree):
    def nodename(x):
        if isinstance(x, basestring):
            return x
        else:
            return x.node

    return (tree.node, tuple(map(nodename, tree[:])))

def list_productions(tree):
    # tree = words_to_indexes(tree)

    def not_terminal(tr):
        return tr.height() > 1

    out = []
    subtrees = tree.subtrees(filter=not_terminal)

    for sub in subtrees:
        out.append(production(sub))
    return out


def production_probability(production, grammar):
    """Look up the probability of the given production in the given grammar."""
    return grammar[production] 

def tree_probability(tree, grammar):
    def product(x,y): return x*y

    out = 1.0
    prods = list_productions(tree)
    probs = [production_probability(prod, grammar) for prod in prods]

    print "Calculating the probability of all of these productions together..."
    for (prod,prob) in zip(prods, probs):
        print "    ",
        print prod, ":", prob

    return reduce(product, probs)

def astronomers():
    print "Trying two different parse trees for:"
    print "'astronomers saw stars with ears'"
    parsetree1 = bracket_parse(
        """
        (S
         (NP astronomers)
         (VP
            (V saw)
            (NP
                (NP stars)
                (PP
                    (P with)
                    (NP ears)))))
        """)

    parsetree2 = bracket_parse(
        """
        (S
         (NP astronomers)
         (VP
            (VP
                (V saw)
                (NP stars))
            (PP
                (P with)
                (NP ears))))
        """)

    grammar = {
        ("S",("NP","VP")): 1.0,
        ("PP", ("P", "NP")): 1.0,
        ("VP", ("V", "NP")): 0.7,
        ("VP", ("VP", "PP")): 0.3,
        ("P", ("with",)): 1.0,
        ("V", ("saw",)): 1.0,

        ("NP", ("NP", "PP")): 0.4,
        ("NP", ("astronomers",)): 0.1,
        ("NP", ("ears",)): 0.18,
        ("NP", ("saw",)): 0.04,
        ("NP", ("stars",)): 0.18,
        ("NP", ("telescopes",)): 0.1
    }

    print tree_probability(parsetree1, grammar)
    print tree_probability(parsetree2, grammar)
        

def henry():
    print "Trying one parse tree for:"
    print "'Henry found walking difficult because he had an abess on his knee'"

    parsetree = bracket_parse(
        """
        (S 
         (NP  (N Henry)) 
         (VP  (V found) 
                (NP (N walking)) 
                (ADJ difficult)) 
         (S  (CONJ  because) 
               (NP  (PRP he)) 
               (VP  (V  had) 
                    (NP  (DET  an) 
                         (N  abess)) 
                    (PP  (P  on) 
                         (NP  (DET  his) 
                              (N  knee))))))
        """)

    grammar = {
        ("S",("NP","VP")): 0.4,
        ("S",("NP","VP","S")): 0.3,
        ("S",("CONJ","NP","VP")): 0.3,

        ("NP", ("DET", "N")): 0.5,
        ("NP", ("N",)): 0.3,
        ("NP", ("PRP",)): 0.2,

        ("PP", ("P", "NP")): 1.0,

        ("VP", ("V",)): 0.3,
        ("VP", ("V","NP","PP")): 0.4,
        ("VP", ("V","NP","ADJ")): 0.3,

        ("N",("Henry",)): 0.1,
        ("N",("abess",)): 0.2,
        ("N",("walking",)): 0.3,
        ("N",("knee",)): 0.4,

        ("V",("found",)): 0.7,
        ("V",("had",)): 0.3,

        ("ADJ",("difficult",)): 1.0,
        ("CONJ",("because",)): 1.0,
        ("PRP",("he",)): 1.0,
        ("DET",("an",)): 0.7,
        ("DET",("his",)): 0.3,
        ("P",("on",)): 1.0
    }

    print tree_probability(parsetree, grammar)

def main():
    astronomers()
    henry()

if __name__ == "__main__": main()
