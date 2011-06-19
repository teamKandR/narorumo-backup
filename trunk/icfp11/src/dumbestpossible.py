#!/usr/bin/env python3

from ltg_util import gameloop
from ltg_util import enqueue_strategy

def main():
    enqueue_strategy("playzero playdec", 10000) # takes 20k turns.
    enqueue_strategy("playzero playsucc playdec", 10000) # 30k.
    enqueue_strategy("playzero playsucc playsucc playdec", 10000) # 40k.

    ## ... just in case they tried healing their slot 255.
    enqueue_strategy("playzero playdec", 10000)
    gameloop()

if __name__ == "__main__": main()
