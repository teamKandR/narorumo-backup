#!/usr/bin/env python3


from ltg_util import gameloop
from ltg_util import enqueue_strategy
from ltg_util import pop_and_print
from ltg_util import get_opponent_move
from ltg_util import apply_card

from ycombinator import build_y_combinator

def build_s_kk_kk():
    apply_slot(5, "K")
    apply_slot(5, "K")
    apply_card("S", 5)
    # TODO...

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
    apply_card("ffff", 0)
    gameloop()

if __name__ == "__main__": main()
