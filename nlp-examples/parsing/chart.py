#!/usr/bin/env python

"""
Common routines for the inside and outside probabilities, working with charts

Alex Rudnick (alex.rudnick@gmail.com)
"""

from collections import defaultdict


def init_table(n):
    """Return the table of betas of size n by n, where each cell contains a
    dictionary from a symbol to the inside probability of the nonterminal
    starting at the row index and continuing to the column index. These are all
    zero by default."""

    def zero(): return 0.0

    return [[defaultdict(zero,{}) for col in range(n)] for row in range(n)]

def init_trace_table(n):
    """Like init_table, only store empty lists by default. We'll populate these
    lists with lists of strings, describing what values the calculation of a
    particular node depends on."""

    def empty(): return []
    return [[defaultdict(empty,{}) for col in range(n)] for row in range(n)]

def printtable(table):
    for i in range(len(table)):
        for j in range(len(table[0])):
            cell = table[i][j]
            for (nonterm, prob) in cell.iteritems():
                if prob:
                    print ("[%d, %d]" % (i+1,j+1)), nonterm,prob

def printtracetable(table, traces):
    for i in range(len(table)):
        for j in range(len(table[0])):
            cell = table[i][j]
            for (nonterm, prob) in cell.iteritems():
                if prob:
                    dependencies = traces[i][j][nonterm]
                    
                    print ("[%d, %d]" % (i+1,j+1)), nonterm,prob,
                    print "depends on:"

                    for dep in dependencies:
                        print "    ",
                        print dep

def printwithindexes(sentence):
    pairs = [str((pos+1,word)) for (pos,word)
        in zip(range(len(sentence)),sentence)]

    print " ".join(pairs)
