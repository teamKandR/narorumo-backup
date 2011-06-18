#!/usr/bin/env python3

from ltg_util import gameloop
from ltg_util import enqueue_strategy
from ltg_util import pop_and_print
from ltg_util import get_opponent_move
from ltg_util import apply_card
from ltg_util import apply_slot
from ltg_util import apply_slotX_to_slotY
from ltg_util import build_num_in_slot

def main():
    build_num_in_slot(3, 7)
    build_num_in_slot(10, 18)
    build_num_in_slot(0, 255)
    build_num_in_slot(3, 0)
    build_num_in_slot(40, 1)

    gameloop()

if __name__ == "__main__": main()
