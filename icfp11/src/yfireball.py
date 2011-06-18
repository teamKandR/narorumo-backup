#!/usr/bin/env python3


from ltg_util import gameloop
from ltg_util import enqueue_strategy
from ltg_util import pop_and_print
from ltg_util import get_opponent_move
from ltg_util import enqueue_left_app

from ycombinator import build_y_combinator

def main():
    build_y_combinator()
    enqueue_left_app("ffff", 0)
    gameloop()

if __name__ == "__main__": main()
