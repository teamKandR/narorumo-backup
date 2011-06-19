#!/usr/bin/env python3

"""
((S ((S (horace (greg (K dec)))) (greg I)))
 ((S ((S (horace (greg (K dec)))) (greg I)))
   ((S ((S (horace (greg (K dec)))) (greg I)))
      ((S ((S (horace (greg (K dec)))) (greg I)))
          ((S (K dec)) I)))))
"""

from ltg_util import gameloop
from ltg_util import enqueue_strategy
from ltg_util import apply_card
from ltg_util import apply_slot
from ltg_util import smash
from ltg_util import copy
from ltg_util import unsafe_copy
from ltg_util import build_num_in_slot
from ltg_friends import build_greg
from ltg_friends import build_horace

def build_onemore():
    """(S ((S (horace (greg (K dec)))) (greg I)))"""
    apply_card("put", 1)
    apply_slot(1, "dec")
    apply_card("K", 1)
    build_greg(0)
    smash()

    # now should have (greg (K dec)) in 0.
    copy(0,1)
    build_horace(0)
    smash()
    apply_card("S", 0)
    # now up to (S (horace (greg (K dec)))) in 0.

    # (greg I) is equivalent to K, so we can just apply slot 0 to K!
    apply_slot(0, "K")

    apply_card("S", 0)

def build_base(slot):
    """((S (K dec)) I)"""
    apply_card("put", slot)
    apply_slot(slot, "dec")
    apply_card("K", slot)
    apply_card("S", slot)
    apply_slot(slot, "I")


## LOOPS is how many times you have to iterate on a single slot, doing 67
## damage each time, to kill it.
LOOPS = 150

def main():
    build_onemore()
    copy(0, 2) ## back up the onemore for later use
    build_base(1)
    smash()
    copy(0,1) ## move the now-bigger function into 1

    ## build up a function that does 67 damage. That's the biggest one that we
    ## can run without going over the function application limit.
    for i in range(65):
        # copy the onemore into 0
        copy(2,0)
        smash()
        # copy the now-bigger function into 1
        copy(0,1)

    ## two backup copies of the 67-damage thing.
    copy(0,2)

    ## step 1: kill their low-numbered slots. (high-numbered for us.)
    build_num_in_slot(253,1)
    smash()
    for targetslot in range(253, 256):
        for i in range(LOOPS):
            ### put the unrolled fireball in slot 0.
            unsafe_copy(2,0)
            ## pass the function a target slot number.
            smash()
        if targetslot != 255:
            apply_card("succ", 1)

    build_num_in_slot(250,1)
    for targetslot in range(250, 253):
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
    for targetslot in range(1, 100):
        apply_card("succ", 1)
        for i in range(LOOPS):
            ### put the unrolled fireball in slot 0.
            unsafe_copy(2,0)
            ## pass the function a target slot number.
            smash()

    gameloop()
if __name__ == "__main__": main()
