#!/usr/bin/env python

"""
Calculates the table of outside probabilities, given the grammar below, for
the (somewhat uncommon) sentence "many antidotes engulf occasionally highly
idealistic peanuts".

Alex Rudnick (alex.rudnick@gmail.com)
"""

from collections import defaultdict
import inside
import chart

def zero(): return 0.0

def outside(rules, words, alphas, betas, traces):
    """Run a dynamic programming algorithm to calculate a chart of all the
    Outside Probabilities; a cell in the table in row i, column j stores the
    alpha value for nonterminals governing from word i to word j. betas is a
    table with the inside probabilities already filled out, and traces is a
    table in which we'll store strings describing what we needed to compute the
    corresponding alpha value in the alpha table."""

    n = len(words)

    ## initialize: the outside probability of the whole sentence is 1.
    alphas[0][n-1]["S"] = 1.0

    # for each possible length of span (of parent nodes)
    for length in range(n, 0, -1):
        # start and end refer to the start and end positions of the parent
        # nodes.
        for start in range(0, n - length + 1):
            end = start + length - 1

            for (parent,children,ruleprob) in rules:
                parent_outside = alphas[start][end][parent]
                parenttrace = traces[start][end][parent]

                if not parent_outside: continue

                for part in range(start, end):
                    if len(children) == 1:
                        pass

                    if len(children) == 2:
                        ## case where child nonterminal of interest is on left
                        beta_right = betas[part+1][end][children[1]]
                        if beta_right:
                            # real calculation
                            alphas[start][part][children[0]] += (
                                parent_outside * ruleprob * beta_right)
                            
                            # store backtrace.
                            traces[start][part][children[0]].append(
                                ("ruleprob (%s -> %s %s) = %f"
                                 % (parent,children[0],children[1], ruleprob)))

                            traces[start][part][children[0]].append(
                                ("beta[%d][%d][%s] = %f" 
                                 % (part+2,end+1,children[1],beta_right)))

                            traces[start][part][children[0]].append(
                                ("alpha[%d][%d][%s] = %f" 
                                 % (start+1,end+1,parent,parent_outside)))

                            # traces[start][part][children[0]] += parenttrace

                        ## case where it's on the right
                        beta_left = betas[start][part][children[0]]
                        if beta_left:
                            # real calculation
                            alphas[part+1][end][children[1]] += (
                                parent_outside * ruleprob * beta_left)

                            # store backtrace.
                            traces[part+1][end][children[1]].append(
                                ("ruleprob (%s -> %s %s) = %f"
                                 % (parent,children[0],children[1],ruleprob)))

                            traces[part+1][end][children[1]].append(
                                ("beta[%d][%d][%s] = %f" 
                                 % (start+1,part+1,children[0],beta_left)))

                            traces[part+1][end][children[1]].append(
                                ("alpha[%d][%d][%s] = %f" 
                                 % (start+1,end+1,parent,parent_outside)))

                            # traces[part+1][end][children[1]] += parenttrace

    print "outside: ok done"

def testsentence(text, rules):
    sentence = text.split()
    chart.printwithindexes(sentence)

    alphas = chart.init_table(len(sentence) + 1)
    betas = chart.init_table(len(sentence) + 1)
    traces = chart.init_trace_table(len(sentence) + 1)

    inside.cyk_betas(rules, sentence, betas)
    outside(rules, sentence, alphas, betas, traces)

    print "INSIDES"
    chart.printtable(betas)

    print
    print "OUTSIDES"
    chart.printtable(alphas)

    print
    print "DEPENDENCIES"
    chart.printtracetable(alphas, traces)

def simple():
    rules = [
("S", ("NP", "VP"), 1.0),
("NP", "Jeremy", 0.5),
("NP", "Jenny", 0.5),
("VP", "sleeps", 0.5),
("VP", "dances", 0.5)]

    text = "Jenny dances"
    testsentence(text, rules)

def astronomers():
    rules = [
("S", ("NP", "VP"), 1.0),
("NP", ("NP", "PP"), 0.4),
("VP", ("V", "NP"), 0.7),
("VP", ("VP", "PP"), 0.3),
("PP", ("P", "NP"), 1.0),

("V", "saw", 1.0),
("NP", "astronomers", 0.1),
("NP", "ears", 0.18),
("NP", "saw", 0.04),
("NP", "stars", 0.18),
("NP", "telescopes", 0.1),
("P", "with", 1.0)]

    text = "astronomers saw stars with ears"
    testsentence(text,rules)

def antidotes():
    rules = [
("S", ("NP", "VP"), 1.0),

("NP", ("DT", "N"), 0.3),
("NP", ("DT", "NP1"), 0.4),
("NP", ("AP", "N"), 0.2),
("NP", ("ADV", "NP1"), 0.1),

("NP1", ("AP", "N"), 0.8),
("NP1", ("AP", "NP1"), 0.2),

("AP", ("ADV", "JJ"), 0.4),
("AP", ("ADV", "AP"), 0.4),
("AP", "idealistic", 0.2),

("VP", ("V", "NP"), 0.5),
("VP", ("V", "NP2"), 0.3),
("VP", ("V", "XP"), 0.2),

("NP2", ("NP", "PP"), 1.0),

("XP", ("ADV", "NP"), 1.0),

("PP", ("P", "NP"), 1.0),

("DT", "many", 1.0),
("N", "peanuts", 0.5),
("N", "antidotes", 0.5),
("V", "engulf", 1.0),
("ADV", "occasionally", 0.5),
("ADV", "highly", 0.5),
("JJ", "idealistic", 0.5),
("JJ", "cute", 0.5)]

    text="many antidotes engulf occasionally highly idealistic peanuts"
    testsentence(text,rules)

def main():
    antidotes()
    # simple()
    # astronomers()

if __name__ == "__main__": main()
