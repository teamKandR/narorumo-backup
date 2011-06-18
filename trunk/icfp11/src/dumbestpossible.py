#!/usr/bin/env python3

import sys

from ltg_util import enqueue_strategy
from ltg_util import pop_and_print
from ltg_util import get_opponent_move

def main():
    whoami = sys.argv[1]
    if whoami == "1":
        get_opponent_move()
        
    # TODO: Take advantage of those 1000 function applications we
    # have.  Do crazy shit.

    # Enqueue instructions to run 10,000 alternations of each
    # strategy.  (We even have some time left at the end.)

    enqueue_strategy("playzero playdec", 10000) # takes 20k turns.

    enqueue_strategy("playzero playsucc playdec", 10000) # 30k.

    enqueue_strategy("playzero playsucc playsucc playdec", 10000) # 40k.

    ## ... just in case they tried healing their slot 255.
    enqueue_strategy("playzero playdec", 10000)

    turn = 0
    while True:
        try:
            # If we have an instruction to execute, do it.
            try:
                pop_and_print()
            except IndexError as e: # got 'em all
                return

            option, card, slot = get_opponent_move()
            turn += 1
        except EOFError as e:
            return

if __name__ == "__main__": main()
