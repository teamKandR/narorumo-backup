#!/usr/bin/env python3


from ltg_util import gameloop
from ltg_util import enqueue_strategy
from ltg_util import pop_and_print
from ltg_util import get_opponent_move
from ltg_util import apply_card
from ltg_util import apply_slot
from ltg_util import apply_slot0_to_slot1
from ltg_util import smash
from ltg_util import copy
from ltg_friends import build_fanny
from ltg_friends import build_greg
from ltg_friends import build_horace
from ltg_friends import build_ian
from ltg_friends import build_june1
from ltg_friends import build_june2
from ltg_friends import build_june
from ltg_friends import build_kelly

from ycombinator import build_y_combinator

def build_rightpart():
    """Build the right part of the fireball function."""
    # build in 1: (K dec)
    apply_card("put", 1)
    apply_slot(1, "dec")
    apply_card("K", 1)

    # build in 0: greg
    build_greg(0)

    # smash together to get (greg (K dec)) in 0
    apply_slot0_to_slot1()

    # copy it to 1.
    apply_card("put", 1)
    apply_slot(1, "zero")
    apply_card("get", 1)

    # build horace in 0.
    build_horace(0)

    # smash together to get (horace (greg (K dec))) in 0.
    apply_slot0_to_slot1()

    # Wrap with an S.
    apply_card("S", 0)

    # build ian in 1.
    build_ian(1)

    # smash together to get ((S (horace (greg (K dec)))) ian) in 0.
    apply_slot0_to_slot1()

def build_leftpart():
    """Build the left part of the fireball function. Doing this uses slots
    0,1,2, and the result will be in slot 0."""
    # build kelly.
    build_kelly()
    # copy kelly to 2.
    copy(0, 2)
    # build june in slots 0,1
    build_june()
    # copy kelly to slot 1
    copy(2, 1)
    # smash together to get (june kelly) in 0
    smash()
    # copy (june kelly) to 1
    copy(0, 1)
    # build horace in 0
    build_horace(0)
    # smash together to get (horace (june kelly)) in 0
    smash()
    # wrap with an S for the whole left part.
    apply_card("S", 0)

def build_fireball():
    """Build the fireball function. We'll apply the Y combinator to it."""
    # build the right part
    build_rightpart()
    # copy it to 3.
    copy(0, 3)

    # build the left part, now it's in 0
    build_leftpart()

    # copy right part from 3 to 1.
    copy(3, 1)
    # smash together for whole fireball.
    smash()

## should look like this when we're done.
# ((S
#   ((S (K S))
#    ((S
#      ((S (K S))
#       ((S ((S (K S)) ((S (K K)) (K S))))
#        ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) I)))))
#     ((S ((S (K S)) ((S (K K)) (K K)))) (K I)))))
#  ((S ((S (K S)) ((S (K K)) (K dec)))) (K I)))

# Or more abstractly, like this:
# ((S
#   (horace
#    ((S
#      (horace
#       ((S (horace (greg (K S))))
#        ((S (horace fanny)) (greg I)))))
#     ((S (horace fanny)) ian))))
#  ((S (horace (greg (K dec)))) ian))

def main():
    build_y_combinator()
    copy(0,4)
    build_fireball()
    copy(0,1)
    copy(4,0)
    smash()
    apply_card("ffff", 200)
    # build_leftpart()
    # build_kelly()
    # build_june()
    gameloop()

if __name__ == "__main__": main()
