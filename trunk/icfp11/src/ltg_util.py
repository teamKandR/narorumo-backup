#!/usr/bin/env python3

import sys

# INSTRUCTION PIPELINING!!
pipeline = []

def build_num_in_slot(num, slot):
    """Build up a number in a slot (to be used as an address,
    potentially)."""

    init_slot_with_card(slot, "zero")

    for i in range(num):
        apply_card("succ", slot)

def apply_slotX_to_slotY(x, y, yaddr=None):
    """
    Result ends up in slot X.

    Example:
    
    We have:
    1={10000,1}
    7={10000,succ}

    Executing apply_slotX_to_slotY(7, 1) leaves us with:

    1={10000,1}
    7={10000,2}
    """
    apply_card("K", x)
    apply_card("S", x)
    apply_slot(x, "get")
    apply_card("K", x)
    apply_card("S", x)

    if (y == 0):
        # I(0) is 0
        apply_slot(x, "I") 
        apply_slot(x, "zero")
    elif (y == 1):
        # resultslot is 1, succ(0) is 1
        apply_slot(x, "succ")
        apply_slot(x, "zero")
    else:
        # Applying a slot to an arbitrary slot number requires some
        # extra work.  If we had, say, a "3" card, then we could apply
        # a slot to slot 3 (or 4) easily.  We don't have a "3" card,
        # but we could have a *slot containing*, say,
        # succ(succ(succ(zero))) == 3.

        # So then, to apply x to slot 3, we'd need to apply slot x to
        # the slot containing 3.  We can do this as long as the slot
        # containing 3 is either 0 or 1!
        assert ((yaddr == 0) or (yaddr == 1))

        if (yaddr == 0):
            # slot 0 contains the slot number we want to apply X to
            apply_slot(x, "I") 
            apply_slotX_to_slotY(x, 0)
        else:
            # slot 1 contains the slot number we want to apply X to
            apply_slot(x, "I")
            apply_slotX_to_slotY(x, 1)

def apply_slot0_to_slot1():
    """Queue up the commands to apply the function in slot 0 to the value,
    which may also be a function, in slot 1. The result will be in slot 0."""
    apply_card("K", 0)
    apply_card("S", 0)
    apply_slot(0, "get")
    apply_card("K", 0)
    apply_card("S", 0)
    apply_slot(0, "succ")
    apply_slot(0, "zero")

def pop_and_print():
    move = pipeline.pop(0)
    print(move[0])
    print(move[1])
    print(move[2])
    
def init_first_x_with_y(x, y):
    """
    Initializes the first X slots with the Yth power of 2, assuming
    they already contain I.

    Uses 2X + YX turns.  For instance initializing the first 3 slots
    with the 2nd power of 2 (that is, 4) would require: zero, zero,
    zero, one, one, one, dbl, dbl, dbl, dbl, dbl, dbl, or 2*3 + 2*3 ==
    12 turns.
    """

    # Get 0s in.  Uses X turns.
    for i in range(x):
        apply_slot(i, "zero")

    # Get 1s in.  Uses X turns.
    for i in range(x):
        apply_card("succ", i)

    # Get bigger numbers in.  Uses X turns for each doubling.
    for i in range(x):
        for j in range(y):
            apply_card("dbl", i)

    # Return the number of turns this takes.
    return 2*x + y*x

def apply_card(card, slot):
    """
    Enqueue an operation that applies card to slot, leaving the result
    in slot.
    """
    pipeline.append(["1", card, slot])

def apply_slot(slot, card):
    """
    Enqueue an operation that applies slot to card, leaving the result
    in slot.
    """
    pipeline.append(["2", slot, card])

def init_slot_with_card(slot, card):
    """
    Just like apply_slot, except that it makes sure that the slot
    contains I first, so that applying the slot to a card simply
    leaves that card in the slot.
    """
    apply_card("put", slot)
    apply_slot(slot, card)

def get_opponent_move():
    option = input().strip()
    one = input().strip()
    two = input().strip()
    if option == "1":
        card, slot = one, two
    elif option == "2":
        card, slot = two, one
    else:
        assert False, "option is neither one nor two?!"
    return option, card, slot

def playzero():
    """
    Enqueue a right application of our slot 0 to the zero card.  When
    run, this will write the value of ((lambda (x) x) 0), which is to
    say, 0, into our slot 0.
    """
    apply_slot(0, "zero")

def playdec(slot=0):
    """
    Enqueue a left application of the dec card to one of our slots
    (default 0).  Assuming that that slot has n in it, this will
    side-effect the opponent's (255-n)th slot, decrementing its
    vitality by 1, and return the identity function, causing our slot
    n to be overwritten by the identity function.
    """
    apply_card("dec", slot)

def playsucc(slot=0):
    """
    Enqueue a left application of the succ card to one of our slots
    (default 0).  Assuming the slot has n in it, the effect of
    playsucc() will be to write n+1 into the slot.  (Then we have an
    n+1 to work with, as an argument to dec, for instance.)
    """

    # Constraints on slot size
    assert (slot >= 0)
    assert (slot <= 255)

    apply_card("succ", slot)

def playdbl(slot=0):
    """
    Enqueue a left application of the dbl card to one of our slots
    (default 0).  Assuming the slot has n in it, the effect of
    playsucc() will be to write n*2 into the slot.
    """

    # Constraints on slot size
    assert (slot >= 0)
    assert (slot <= 255)

    apply_card("dbl", slot)

commands = {
    "playzero" : playzero,
    "playdec" : playdec,
    "playsucc" : playsucc,
    "playdbl" : playdbl,
}

def enqueue_strategy(cmds, times):
    # On any given turn, we're only going to use one of the commands
    # in cmds.  Pick the right one out of the list based on which
    # of our turns it is.

    """
    Example: if there are 3 cmds and we want them run 10000 times,
    then we have 30000 things to enqueue.
    """
    splitted = cmds.split()
    num_cmds = len(splitted)
    for i in range(times * num_cmds):
        # Pick the corresponding function out of the commands dict.
        funk = commands[splitted[i % num_cmds]]
        funk()

def gameloop():
    """Main loop for our default agents."""
    
    whoami = sys.argv[1]
    if whoami == "1":
        get_opponent_move()

    turn = 0
    while True:
        try:
            # If we have an instruction to execute, do it.
            try:
                pop_and_print()
            except IndexError as e: # got 'em all
                return
            option, card, slot = get_opponent_move()
            turn += 1
        except EOFError as e:
            return
