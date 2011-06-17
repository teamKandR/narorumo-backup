#!/usr/bin/env python3

import sys

turn_queue = []

def run_all(tq):
    while True:
        try:
            pop_and_print(tq)
        except IndexError as e: # got 'em all
            return

def pop_and_print(tq):
    move = tq.pop(0)
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
        enqueue_right_app(i, "zero")

    # Get 1s in.  Uses X turns.
    for i in range(x):
        enqueue_left_app("succ", i)

    # Get bigger numbers in.  Uses X turns for each doubling.
    for i in range(x):
        for j in range(y):
            enqueue_left_app("dbl", i)

    # Return the number of turns this takes.
    return 2*x + y*x

def enqueue_left_app(card, slot):
    """
    Enqueue an operation that applies card to slot, leaving the result
    in slot.
    """
    turn_queue.append(["1", card, slot])

def enqueue_right_app(slot, card):
    """
    Enqueue an operation that applies slot to card, leaving the result
    in slot.
    """
    turn_queue.append(["2", slot, card])

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
    enqueue_right_app(0, "zero")

def playdec(slot=0):
    """
    Enqueue a left application of the dec card to one of our slots
    (default 0).  Assuming that that slot has n in it, this will
    side-effect the opponent's (255-n)th slot, decrementing its
    vitality by 1, and return the identity function, causing our slot
    n to be overwritten by the identity function.
    """
    enqueue_left_app("dec", slot)

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

    enqueue_left_app("succ", slot)

def playdbl(slot=0):
    """
    Enqueue a left application of the dbl card to one of our slots
    (default 0).  Assuming the slot has n in it, the effect of
    playsucc() will be to write n*2 into the slot.
    """

    # Constraints on slot size
    assert (slot >= 0)
    assert (slot <= 255)

    enqueue_left_app("dbl", slot)

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

def main():
    whoami = sys.argv[1]
    if whoami == "1":
        get_opponent_move()

        
    # TODO: Take advantage of those 1000 function applications we
    # have.  Do crazy shit.

    turn = 0
    while True:
        try:
            # Remember, turns in the actual game are numbered from 1,
            # not 0.
            
            # Write 16 into the first 5 slots.  Side-effects the turn
            # queue.
            init_first_x_with_y(5, 4)

            # Enqueue 10,000 alternations of each strategy.  (We won't
            # get to all of these.)
            enqueue_strategy("playzero playdec", 10000)
            enqueue_strategy("playzero playsucc playdec", 10000)
            enqueue_strategy("playzero playsucc playsucc playdec", 10000)

            # Run 'em all.
            run_all(turn_queue)

            option, card, slot = get_opponent_move()
            turn += 1
        except EOFError as e:
            return

if __name__ == "__main__": main()
