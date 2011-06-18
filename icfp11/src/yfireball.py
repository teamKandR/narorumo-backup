#!/usr/bin/env python3


from ltg_util import gameloop
from ltg_util import enqueue_strategy
from ltg_util import pop_and_print
from ltg_util import get_opponent_move
from ltg_util import apply_card
from ltg_util import apply_slot

from ycombinator import build_y_combinator

def build_fanny():
    # TODO...
    pass

def build_greg():
    # TODO...
    pass

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
 #   ((S (K S))
 #    ((S
 #      ((S (K S))
 #       ((S ((S (K S)) ((S (K K)) (K S))))
 #        ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) I)))))
 #     ((S ((S (K S)) ((S (K K)) (K K)))) (K I)))))
 #  ((S ((S (K S)) ((S (K K)) (K dec)))) (K I)))

def main():
    build_y_combinator()
    build_horace(1)
    build_ian(2)
    apply_card("ffff", 0)
    gameloop()

if __name__ == "__main__": main()
