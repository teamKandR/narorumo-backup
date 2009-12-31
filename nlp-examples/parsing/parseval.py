#!/usr/bin/env python

"""
Calculates parsing evaluation metrics: precision, recall, labeled precision and
labeled recall.

Depends on nltk (developed with version 2.05b)

Alex Rudnick (alex.rudnick@gmail.com)
"""

from nltk.tree import *
import copy

def precision(gold, parse, ignore_labels=True):
    """Return the proportion of brackets in the suggested parse tree that are
    in the gold standard."""

    parsebrackets = list_brackets(parse)
    goldbrackets = list_brackets(gold)

    parsebrackets_u = list_brackets(parse, ignore_labels=True)
    goldbrackets_u = list_brackets(gold, ignore_labels=True)

    if ignore_labels:
        candidate = parsebrackets_u
        gold = goldbrackets_u
    else:
        candidate = parsebrackets
        gold = goldbrackets

    total = len(candidate)
    successes = 0
    for bracket in candidate:
        if bracket in gold:
            successes += 1
    return float(successes) / float(total)

def recall(gold, parse, ignore_labels=True):
    """Return the proportion of brackets in the gold standard that are in the
    suggested parse tree."""

    parsebrackets = list_brackets(parse)
    goldbrackets = list_brackets(gold)

    parsebrackets_u = list_brackets(parse, ignore_labels=True)
    goldbrackets_u = list_brackets(gold, ignore_labels=True)

    if ignore_labels:
        candidate = parsebrackets_u
        gold = goldbrackets_u
    else:
        candidate = parsebrackets
        gold = goldbrackets

    total = len(gold)
    successes = 0
    for bracket in gold:
        if bracket in candidate:
            successes += 1
    return float(successes) / float(total)

def labeled_precision(gold, parse):
    return precision(gold, parse, ignore_labels=False)

def labeled_recall(gold, parse):
    return recall(gold, parse, ignore_labels=False)

def words_to_indexes(tree):
    """Return a new tree based on the original tree, such that the leaf values
    are replaced by their indexs."""

    out = copy.deepcopy(tree)

    leaves = out.leaves()
    for index in range(0, len(leaves)):
        path = out.leaf_treeposition(index)

        out[path] = index + 1
    return out

def firstleaf(tr):
    return tr.leaves()[0]

def lastleaf(tr):
    return tr.leaves()[-1]

def list_brackets(tree, ignore_labels=False):
    tree = words_to_indexes(tree)

    def not_pos_tag(tr):
        return tr.height() > 2

    def label(tr):
        if ignore_labels:
            return "ignore"
        else:
            return tr.node

    out = []
    subtrees = tree.subtrees(filter=not_pos_tag)
    return [(firstleaf(sub), lastleaf(sub), label(sub)) for sub in subtrees]

def example():
    gold = bracket_parse(
"""
(PX
    (PX
        (APPR an)
        (NX
            (ART einem)
            (NX
                (NX (NN Samstag))
                (KON oder)
                (NX (NN Sonntag)))))
    (ADVX (ADV vielleicht)))
""")

    parse = bracket_parse(
"""
(PX
    (PX
        (APPR an)
        (NX
            (ART einem)
            (NN Samstag)))
    (NX (KON oder) (NX (NN Sonntag)))
    (ADVX (ADV vielleicht)))
""")
    
    pscore = precision(gold,parse)
    rscore = recall(gold,parse)
    print pscore, rscore

def problem2():
    gold = bracket_parse(
"""
(SIMPX
    (VF
        (PX (appr von)
            (NX
                (pidat allen)
                (ADJX (adja kulturellen))
                (nn Leuchttuermen))))

    (LK
        (VXFIN (vxfin besitzt)))

    (MF
        (ADVX
            (ADVX (adv nach))
            (kon wie)
            (ADVX (adv vor)))
        (NX
            (art das)
            (nn Theater))
        (NX
            (art das)
            (ADJX (adja unsicherste))
            (nn Fundament))))
""")

    parse = bracket_parse(
"""
(R-SIMPX
    (LV
        (PX
            (appr von)
            (NX (pidat allen))))

    (VF
        (NX
            (ADJX (adja kulturellen))
            (nn Leuchttuermen)))
    
    (LK (VXFIN (vvfin besitzt)))

    (MF
        (PX
            (PX (appr nach))
            (kon wie)
            (PX
                (appr vor)
                (NX
                    (art das)
                    (nn Theater))))
        (NX
            (art das)
            (ADJX (adja unsicherste))
            (nn Fundament))))
""")
    print "Precision:", precision(gold,parse)
    print "Labeled precision:", labeled_precision(gold,parse)
    print "Recall:", recall(gold,parse)
    print "Labeled recall:", labeled_recall(gold,parse)

def main():
    # example()
    problem2()

if __name__ == "__main__": main()
