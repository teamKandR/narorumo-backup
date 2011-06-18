#!/usr/bin/env python3

from ltg_util import apply_slot0_to_slot1
from ltg_util import enqueue_left_app
from ltg_util import enqueue_right_app

def build_clarice(slot):
    enqueue_right_app(slot,"S")
    enqueue_left_app("K",slot)
    enqueue_left_app("S",slot)
    enqueue_right_app(slot,"K")
    enqueue_left_app("S",slot) # and now the S around clarice.

def build_dianne(slot):
    enqueue_left_app("S",slot)
    enqueue_right_app(slot,"I")
    enqueue_left_app("K",slot)

def build_abe(slot):
    enqueue_right_app(slot,"S")
    enqueue_right_app(0,"I")
    enqueue_right_app(0,"I")
    enqueue_left_app("K",0)
    enqueue_left_app("S",0)

def build_y_combinator():
    """Build the y combinator into slot 0. Assumes all the slots are currently
    filled with I, but only touches slots 0 and 1."""

    # Build clarice in slot 0.
    build_clarice(0)

    # Build dianne in slot 1.
    build_dianne(1)

    # Apply slot 0 to slot 1; and now we have benny in slot 0.
    apply_slot0_to_slot1()

    # Now put a 0 in slot 1, and use "get": benny is in slot 1.
    enqueue_left_app("put", 1)
    enqueue_right_app(1, "zero")
    enqueue_left_app("get", 1)

    # Now build abe with an extra S in slot 0.
    build_abe(0)

    # Apply slot 0 to slot 1, and we have the Y combinator in slot 0.
    apply_slot0_to_slot1()
