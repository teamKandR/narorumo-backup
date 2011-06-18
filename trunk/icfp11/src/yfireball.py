#!/usr/bin/env python3


from ltg_util import gameloop
from ltg_util import enqueue_strategy
from ltg_util import pop_and_print
from ltg_util import get_opponent_move
from ltg_util import apply_card
from ltg_util import apply_slot
from ltg_util import apply_slot0_to_slot1

from ycombinator import build_y_combinator

def build_fanny():
    """((S (K K)) (K K)).
    Uses slots 0 and 1, result ends up in slot 0."""
    apply_card("put", 1)
    apply_slot(1, "K")
    apply_card("K", 1)
    build_greg(0)
    apply_slot0_to_slot1()

def build_greg(slot):
    """(S (K K))"""
    apply_card("put", slot)
    apply_slot(slot, "K")
    apply_card("K", slot)
    apply_card("S", slot)

def build_horace(slot):
    """(S (K S))"""
    apply_card("put", slot)
    apply_slot(slot, "S")
    apply_card("K", slot)
    apply_card("S", slot)

def build_ian(slot):
    """(K I)"""
    apply_card("put", slot)
    apply_card("K", slot)

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

def build_fireball():
    """Build the fireball function. We'll apply the Y combinator to it."""
    pass

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
    # build_y_combinator()
    build_rightpart()
    gameloop()

if __name__ == "__main__": main()
