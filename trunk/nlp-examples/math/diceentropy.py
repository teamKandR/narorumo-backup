#!/usr/bin/env python

"""
Quick demonstration of calculating the entropy of a discrete random variable.
In this case, we show the entropy of a weighted die versus a fair die.
"""

import math

def entropy(probs):
    """Given a list of the probabilities for each value of a random variable,
    calculate the entropy of that random variable."""

    return -1.0 * sum( [(math.log(p,2) * p) for p in probs] )

def main():
    # this hypothetical unfair die is weighted where even numbers are twice as
    # likely to come up.
    unfair_dice_probs = [1.0/9, 2.0/9, 1.0/9, 2.0/9, 1.0/9, 2.0/9]
    fair_dice_probs = [1.0/6] * 6

    print """\
The random variable describing the outcomes of a weighted six-sided die has
entropy: """
    print entropy(unfair_dice_probs)

    print "Whereas a fair six-sided die's RV has entropy: "
    print entropy(fair_dice_probs)

if __name__ == "__main__": main()
