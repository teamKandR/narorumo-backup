#!/usr/bin/env python3

"""
Build something like a linked list of decs and apply it repeatedly!!
It actually ends up looking like
SSSS...decdecdecdec....
"""

from ltg_util import gameloop
from ltg_util import enqueue_strategy
from ltg_util import pop_and_print
from ltg_util import get_opponent_move
from ltg_util import apply_card
from ltg_util import apply_slot
from ltg_util import apply_slotX_to_slotY
from ltg_util import smash
from ltg_util import copy
from ltg_util import unsafe_copy
from ltg_util import build_num_in_slot
from ltg_friends import build_greg
from ltg_friends import build_horace

def build_linkedlist_better():
    """Builds the linked list of dec in slot 0."""
    ## build the stuff.
    apply_slot(0, "S")
    apply_slot(0,"dec")
    apply_slot(0,"dec")
    for i in range(330):
        apply_card("S", 0)
        apply_slot(0,"dec")

## LOOPS is how many times you have to iterate on a single slot, doing 332
## damage each time, to kill it.
LOOPS = 31

def main():
    ## build the damage function.
    build_linkedlist_better()

    # two backup copies of the function.
    copy(0, 1)
    copy(0, 2)

    ## step 1: kill their low-numberd slots. (high-numbered for us.)
    build_num_in_slot(254,1)
    smash()
    for targetslot in range(254, 256):
        for i in range(LOOPS):
            ### put the unrolled fireball in slot 0.
            unsafe_copy(2,0)
            ## pass the function a target slot number.
            smash()
        if targetslot != 255:
            apply_card("succ", 1)

    build_num_in_slot(250,1)
    for targetslot in range(250, 254):
        for i in range(LOOPS):
            ### put the unrolled fireball in slot 0.
            unsafe_copy(2,0)
            ## pass the function a target slot number.
            smash()
        if targetslot != 253:
            apply_card("succ", 1)

    unsafe_copy(2,0)
    copy(2,1)
    ## now start working on slot 0, which is easy to attack.
    apply_slot(0, "zero")
    for i in range(LOOPS):
        unsafe_copy(1,0)
        ## pass slot 0 a slot number.
        apply_slot(0, "zero")

    build_num_in_slot(0, 1)
    ## really should be up to range 256. How fast are we?
    for targetslot in range(1, 256):
        apply_card("succ", 1)
        for i in range(LOOPS):
            ### put the unrolled fireball in slot 0.
            unsafe_copy(2,0)
            ## pass the function a target slot number.
            smash()
    gameloop()
if __name__ == "__main__": main()
