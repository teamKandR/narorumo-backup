#!/usr/bin/env python

"""
Solve cryptoarithmetic puzzles in a pretty naive way, with constraint
programming.
"""

from __future__ import division
from __future__ import print_function

from constraint import AllDifferentConstraint
from constraint import Problem
import sys

class LettersAddUp(object):
    def __init__(self, terms, total, letters):
        self.terms = terms
        self.total = total
        self.letters = letters

    def __call__(self, *values):
        termtotal = 0
        assignments = {}
        for k,v in zip(self.letters, values):
            assignments[k] = v
            
        for term in self.terms:
            place = 1
            mytotal = 0
            for letter in reversed(term):
                mytotal += place * assignments[letter]
                place *= 10
            termtotal += mytotal

        totaltotal = 0
        place = 1
        for letter in reversed(self.total):
            totaltotal += place * assignments[letter]
            place *= 10
        return totaltotal == termtotal

class DigitCheckout(object):
    def __init__(self, totalletter, components, letters):
        """components is a list like [['d',e'],['n','r']]"""
        self.totalletter = totalletter
        self.components = components
        self.letters = letters

    def __call__(self, *values):
        assignments = {}
        for k,v in zip(self.letters, values):
            assignments[k] = v
        
        totaldigit = assignments[self.totalletter]
        runningtotal = 0
        for i in range(len(self.components)):
            runningtotal //= 10
            runningtotal += sum([assignments[x] for x in self.components[i]])
        runningtotal %= 10
        return runningtotal == totaldigit

def digit_checks(problem, terms, total, letters):
    shortest = min(map(len, terms))
    reversedterms = map(lambda term: list(reversed(term)), terms)
    reversedtotal = list(reversed(total))

    for end in range(min(3, shortest)):
        problem.addConstraint(
            DigitCheckout(reversedtotal[end],
                          [[term[i] for term in reversedterms
                                    if i in range(len(term))]
                           for i in range(end + 1)], letters),
                           letters)

def buildproblem(terms, total):
    problem = Problem()

    letters = sorted(list(set("".join(terms) + total)))
    initial = set([word[0] for word in terms + [total]])

    for letter in letters:
        if letter in initial:
            problem.addVariable(letter, list(range(1,10)))
        else:
            problem.addVariable(letter, list(range(10)))
    problem.addConstraint(AllDifferentConstraint())
    digit_checks(problem, terms, total, letters)
    problem.addConstraint(LettersAddUp(terms,total,letters), letters)

    return problem

def main():
    if len(sys.argv) <= 2:
        print("usage: {0} send more money".format(sys.argv[0]))
        return

    terms = sys.argv[1:-1]
    total = sys.argv[-1]
    print("sum({0}) == {1}".format(terms, total))

    problem = buildproblem(terms, total)
    soln = problem.getSolution()
    print(soln)
    for term in terms:
        print(" " * (len(total) - len(term)), end="")
        print(term, "  ", map(lambda x: soln[x], list(term)))
    print(total, map(lambda x: soln[x], list(total)))

if __name__ == "__main__": main()
