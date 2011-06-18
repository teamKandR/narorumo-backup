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
    # Set up address of slot 3, in slot 0.
    build_num_in_slot(3, 0)

    # Set up contents of slot 3.
    build_num_in_slot(50, 3)

    # Set up address of slot 255, in slot 1.
    build_num_in_slot(255, 1)

    # Set up contents of slot 255.
    build_num_in_slot(20, 255)

    apply_card("put", 5)
    apply_slot(5, "succ")

    # Apply succ, in slot 5, to 50, in slot 3.  Result should be 51,
    # and end up in slot 5.
    apply_slotX_to_slotY(5, 3, yaddr=0)

    apply_card("put", 6)
    apply_slot(6, "dbl")

    # Apply dbl, in slot 6, to 20, in slot 255.  Result should be 40,
    # and end up in slot 6.
    apply_slotX_to_slotY(6, 255, yaddr=1)

    gameloop()

if __name__ == "__main__": main()
