#!/usr/bin/env python3

"""
((S ((S (horace (greg (K dec)))) (greg I)))
 ((S ((S (horace (greg (K dec)))) (greg I)))
   ((S ((S (horace (greg (K dec)))) (greg I)))
      ((S ((S (horace (greg (K dec)))) (greg I)))
          ((S (K dec)) I)))))

Our strategy this time:


"""

from ltg_util import gameloop
from ltg_util import enqueue_strategy
from ltg_util import pipeline
from ltg_util import pop_and_print
from ltg_util import get_opponent_move
from ltg_util import apply_card
from ltg_util import apply_slot
from ltg_util import apply_slot0_to_slot1
from ltg_util import smash
from ltg_util import copy
from ltg_util import unsafe_copy
from ltg_util import build_num_in_slot
from ltg_friends import build_greg
from ltg_friends import build_horace

def main():

    # Decrements all the other player's slots by 8 within 17,000 turns
    # or so.  Maybe we can exploit this?

    # Put "((S dec) dec)" in slot 1.
    apply_slot(1, "dec")
    apply_card("S", 1)
    apply_slot(1, "dec")

    # Build an S in slot 0.
    apply_slot(0, "S")

    # # Put (S ((S dec) dec)) in slot 0.
    smash()

    # Put (S ((S dec) dec)) and ((S dec) dec) together in slot 0.
    smash()

    # Slot 0 contains: ((S ((S dec) dec)) ((S dec) dec))
    # Copy it over to slot 1 as well
    copy(0,1)

    # # Now do: (((S slot0) slot1) targetslot)
    apply_card("S", 0)
    smash()

    num = 0;
    while (len(pipeline) < 17000):
        # Save for later
        copy(0,2)

        build_num_in_slot(num, 1)
        smash()

        # Copy back to slot 0
        copy(2,0)

        num = num + 1

    gameloop()
if __name__ == "__main__": main()
