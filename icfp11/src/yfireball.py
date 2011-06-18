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

# We want this:
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
    build_fanny()
    build_greg(11)
    build_horace(12)
    build_ian(13)
    apply_card("ffff", 0)
    gameloop()

if __name__ == "__main__": main()
