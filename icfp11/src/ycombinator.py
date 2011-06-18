#!/usr/bin/env python3

from ltg_util import apply_slot0_to_slot1
from ltg_util import apply_card
from ltg_util import apply_slot

def build_clarice(slot):
    apply_slot(slot,"S")
    apply_card("K",slot)
    apply_card("S",slot)
    apply_slot(slot,"K")
    apply_card("S",slot) # and now the S around clarice.

def build_dianne(slot):
    apply_card("S",slot)
    apply_slot(slot,"I")
    apply_card("K",slot)

def build_abe(slot):
    apply_slot(slot,"S")
    apply_slot(0,"I")
    apply_slot(0,"I")
    apply_card("K",0)
    apply_card("S",0)

def build_y_combinator():
    """Build the y combinator into slot 0. Only touches slots 0 and 1."""

    apply_card("put",0)
    apply_card("put",1)

    # Build clarice in slot 0.
    build_clarice(0)

    # Build dianne in slot 1.
    build_dianne(1)

    # Apply slot 0 to slot 1; and now we have benny in slot 0.
    apply_slot0_to_slot1()

    # Now put a 0 in slot 1, and use "get": benny is in slot 1.
    apply_card("put", 1)
    apply_slot(1, "zero")
    apply_card("get", 1)

    # Now build abe with an extra S in slot 0.
    build_abe(0)

    # Apply slot 0 to slot 1, and we have the Y combinator in slot 0.
    apply_slot0_to_slot1()
