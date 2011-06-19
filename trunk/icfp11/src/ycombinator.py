#!/usr/bin/env python3

from ltg_util import apply_slot0_to_slot1
from ltg_util import apply_card
from ltg_util import apply_slot
from ltg_util import copy

from ltg_friends import build_abe
from ltg_friends import build_clarice
from ltg_friends import build_dianne
from ltg_friends import build_greg
from ltg_friends import build_horace
from ltg_friends import build_june1
from ltg_friends import build_kelly
from ltg_friends import build_ian

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
    # Now copy benny to slot 1.
    copy(0, 1)
    # Build abe with an extra S in slot 0.
    build_abe(0)
    # Apply slot 0 to slot 1, and we have the Y combinator in slot 0.
    apply_slot0_to_slot1()

def build_applicative_y_combinator(slotnum):
    """
    Build the applicative-order Y combinator in slot 'slotnum'.  Only
    touches slots 'slotnum' and 'slotnum'+1.
    """

    # TODO: build this.
# ((S 
# ((S (horace (greg I))) ((S (horace (june1 ((S (horace (june1 kelly))) kelly)))) (greg ian))))
# ((S (horace (greg I))) ((S (horace (june1 ((S (horace (june1 kelly))) kelly)))) (greg ian)))))

    
    
    
    
    
    
