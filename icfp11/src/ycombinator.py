#!/usr/bin/env python3

from ltg_util import apply_slotX_to_slotY
from ltg_util import build_num_in_slot
from ltg_util import apply_card
from ltg_util import apply_slot
from ltg_util import copy
from ltg_util import smash

from ltg_friends import build_abe
from ltg_friends import build_clarice
from ltg_friends import build_dianne
from ltg_friends import build_greg
from ltg_friends import build_horace
from ltg_friends import build_june1
from ltg_friends import build_kelly
from ltg_friends import build_ian

def build_applicative_y(n):
    """
    Build the applicative-order Y combinator into slot n.  Warning:
    touches slots 0 and 1 as well as slots n, n+1, and n+2.

    Supposedly, what we're building is:

    ((S 
      ((S (horace (greg I))) 
       ((S (horace (june1 ((S (horace (june1 kelly))) kelly)))) (greg ian))))
     ((S (horace (greg I))) 
      ((S (horace (june1 ((S (horace (june1 kelly))) kelly)))) (greg ian)))))

    """

    # Put (june1 kelly) in slot n.
    build_june1()
    copy(0, n) # move june1 to slot n
    build_kelly()
    apply_slotX_to_slotY(n, 0)

    # Put address of n in slot 0.
    build_num_in_slot(n, 0)

    # Put (horace (june1 kelly)) in slot n+1.
    build_horace(n+1)
    apply_slotX_to_slotY(n+1, n, yaddr=0)

    # Put (S (horace (june1 kelly))) in slot n+1.
    apply_card("S", n+1)

    # Put ((S (horace (june1 kelly))) kelly) in slot n+1.
    build_kelly() # always ends up in slot 0.
    apply_slotX_to_slotY(n+1, 0)

    # Put june1 in slot 0.
    build_june1()

    # Put address of n+1 in slot 1.
    build_num_in_slot(n+1, 1)

    # Put (june1 ((S (horace (june1 kelly))) kelly)) in slot 0.
    apply_slotX_to_slotY(0, n+1, yaddr=1)

    # Put horace in slot n.
    build_horace(n)

    # Put (horace (june1 ((S (horace (june1 kelly))) kelly))) in slot n.
    apply_slotX_to_slotY(n, 0)

    # Put (S (horace (june1 ((S (horace (june1 kelly))) kelly)))) in slot n.
    apply_card("S", n)

    # Put (greg ian) in slot 0.
    build_greg(0)
    build_ian(1)
    smash()

    # Put 
    # ((S (horace (june1 ((S (horace (june1 kelly))) kelly)))) (greg ian)) 
    # in slot n.
    apply_slotX_to_slotY(n, 0)
    
    # Put (greg I) in slot n+1.
    # It turns out (greg I) is equivalent to K!
    apply_card("put", n+1)
    apply_slot(n+1, "K")

    # Put (horace (greg I)) in slot n+2.
    build_horace(n+2)
    # Put address of n+1 in slot 1.
    build_num_in_slot(n+1, 1)
    apply_slotX_to_slotY(n+2, n+1, yaddr=1)

    # Put (S (horace (greg I))) in slot n+2.
    apply_card("S", n+2)

    # Put
    # ((S (horace (greg I))) 
    #  ((S (horace (june1 ((S (horace (june1 kelly))) kelly)))) (greg ian)))
    # in slot n+2.
    build_num_in_slot(n, 0)
    apply_slotX_to_slotY(n+2, n, yaddr=0)

    # Copy slot n+2 to slot n+1.
    copy(n+2, n+1)

    # Apply S to slot n+2, leaving the result in slot n+2.
    apply_card("S", n+2)

    # Apply contents of n+2 to contents of n+1, leaving the result in
    # slot n+2.
    #build_num_in_slot(n+1, 1)
    apply_slotX_to_slotY(n+2, n+1, yaddr=1)

    # Copy slot n+2 to slot n.
    copy(n+2, n)
