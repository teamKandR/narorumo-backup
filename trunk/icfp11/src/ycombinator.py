#!/usr/bin/env python3

from ltg_util import apply_slot0_to_slot1
from ltg_util import apply_slotX_to_slotY
from ltg_util import build_num_in_slot
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

    # Put address of n in slot 0, and address of n+1 in slot 1.
    build_num_in_slot(n, 0)
    build_num_in_slot(n+1, 1)

    # Put (horace (june1 kelly)) in slot n+1.
    build_horace(n+1)
    apply_slotX_to_slotY(n+1, n, yaddr=0)

    # Put (S (horace (june1 kelly))) in slot n+1.
    apply_card("S", n+1)

    # Put ((S (horace (june1 kelly))) kelly) in slot n+1.
    build_kelly() # always ends up in slot 0.
    apply_slotX_to_slotY(0, n, yaddr=0)

    # Put june1 in slot 0.
    build_june1()

    # Put address of n in slot 0, and address of n+1 in slot 1.  (Have
    # to redo this because build_june1 clobbered them.)
    build_num_in_slot(n, 0)
    build_num_in_slot(n+1, 1)

    # Put (june1 ((S (horace (june1 kelly))) kelly)) in slot n+1.
    apply_slotX_to_slotY(0, n+1, yaddr=1)

    # Put horace in slot n.
    build_horace(n)

    # Put (horace (june1 ((S (horace (june1 kelly))) kelly))) in slot n+1.
    apply_slotX_to_slotY(n, n+1, yaddr=1)

    # Put (S (horace (june1 ((S (horace (june1 kelly))) kelly)))) in slot n+1.
    apply_card("S", n+1)

    # Put (greg ian) in slot n+2.
    build_greg(n+2)
    build_ian(n)
    apply_slotX_to_slotY(n+2, n, yaddr=0)

    # Put 
    # ((S (horace (june1 ((S (horace (june1 kelly))) kelly)))) (greg ian)) 
    # in slot n+1.
    apply_card("succ", 1) # bump up address count in slot 1 first, so
                          # it refers to slot n+2.
    apply_slotX_to_slotY(n+1, n+2, yaddr=1)
    
    # Put (greg I) in slot n+2.
    apply_slot(n+2, "I") # greg is still in slot n+2 at this point

    # Put (horace (greg I)) in slot n+2.
    apply_slotX_to_slotY(n, n+2, yaddr=1) # horace is still in slot n
                                          # at this point

    # Put (S (horace (greg I))) in slot n+2.
    apply_card("S", n+2)

    # Put
    # ((S (horace (greg I))) 
    #  ((S (horace (june1 ((S (horace (june1 kelly))) kelly)))) (greg ian)))
    # in slot n+2.
    build_num_in_slot(n+1, 1) # redo this so slot 1 refers to slot n+1
                              # again
    apply_slotX_to_slotY(n+2, n+1, yaddr=1)

    # Copy slot n+2 to slot n+1.
    copy(n+2, n+1)

    # Apply S to slot n+2, leaving the result in slot n+2.
    apply_card("S", n+2)

    # Apply contents of n+2 to contents of n+1, leaving the result in
    # slot n+2.
    apply_slotX_to_slotY(n+2, n+1, yaddr=1)

    # Copy slot n+2 to slot n.
    copy(n+2, n)
    
    
    
    
    
    
    
