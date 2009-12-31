#!/usr/bin/env python

"""
Calculates the table of inside probabilities, given the grammar below, for the
sentence "a dog ate pizza with a fork in a kitchen".

Alex Rudnick (alex.rudnick@gmail.com)
"""

import chart

def cyk_betas(rules, words, table):
    """Run a CYK-style algorithm to calculate a chart of all the Inside
    Probabilities; a cell in the table in row i, column j stores the beta value
    for nonterminals happening, governing from word i to word j."""

    n = len(words)

    ## initialize: the one-word case
    for i in range(n):
        for (left, right, prob) in rules:
            # unit production Rj -> ai, set P[i,1,j] = true.
            if right == words[i]:
                table[i][i][left] = prob

    ## build up the table!

    # for each length of span
    for length in range(1, n+1):

        # for each start of span
        for start in range(0, n - length + 1):
            end = start + length - 1

            # for each partition of span
            for part in range(start, end + 1):

                # For each production RA -> RB RC
                for left,right,ruleprob in rules:

                    # skip rules that go to terminals
                    if isinstance(right, basestring): continue

                    # end of the constituent on the LHS; the mother node.
                    if len(right) == 1:
                        if (table[start][end][right[0]]):
                            childbeta = table[start][end][right[0]]

                            table[start][end][left] += ruleprob * childbeta

                    elif len(right) == 2: 
                        if (table[start][part][right[0]] and
                            table[part+1][end][right[1]]):

                            leftbeta = table[start][part][right[0]]
                            rightbeta = table[part+1][end][right[1]]

                            table[start][end][left] += (ruleprob * leftbeta *
                                                        rightbeta)
    print "inside: ok done"
    return table

def dog_pizza():
    """The dog eating the pizza example."""

    rules = [
("S", ("NP", "VP"), 1.0),
("NP", ("DET", "N"), 0.4),
("NP", ("N",), 0.3),
("NP", ("NP", "PP"), 0.3),
("VP", ("V", "NP"), 0.8),
("VP", ("V", "XP"), 0.2),
("PP", ("IN", "NP"), 1.0),
("XP", ("NP", "PP"), 1.0),

("DET", "a", 1.0),
("N", "dog", 0.1),
("N", "pizza", 0.2),
("N", "fork", 0.4),
("N", "kitchen", 0.3),
("V", "ate", 1.0),
("IN", "with", 0.4),
("IN", "in", 0.6)]
    
    sentence = "a dog ate pizza with a fork in a kitchen".split()
    chart.printwithindexes(sentence)

    table = chart.init_table(len(sentence) + 1)
    cyk_betas(rules, sentence, table)

    chart.printtable(table)

def main():
    dog_pizza()

if __name__ == "__main__": main()
