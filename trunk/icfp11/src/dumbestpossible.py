#!/usr/bin/env python3

import sys

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
    Do a right application of our slot 0 to the zero card.  This
    writes the value of ((lambda (x) x) 0), which is to say, 0, into
    our slot 0.
    """
    print("2")
    print("0")
    print("zero")

def playdec(slot=0):
    """
    Do a left application of the dec card to one of our slots (default
    0).  Assuming the slot has n in it, This side-effects the
    opponent's (255-n)th slot, decrementing its vitality by 1, and
    returns the identity function, causing our slot n to be
    overwritten by the identity function.
    """
    print("1")
    print("dec")
    print(slot)

def playsucc(slot=0):
    """
    Do a left application of the succ card to one of our slots
    (default 0).  Assuming the slot has n in it, the effect of
    playsucc() is to write n+1 into the slot.  (Then we have an n+1 to
    work with, as an argument to dec, for instance.)
    """

    # Constraints on slot size
    assert (slot >= 0)
    assert (slot <= 255)

    print("1")
    print("succ")
    print(slot)

def playdbl(slot=0):
    """
    Do a left application of the dbl card to one of our slots
    (default 0).  Assuming the slot has n in it, the effect of
    playsucc() is to write n*2 into the slot.
    """

    # Constraints on slot size
    assert (slot >= 0)
    assert (slot <= 255)

    print("1")
    print("dbl")
    print(slot)

def get_single_action(turns, action, current_turn):
    # Say the current turn is 1, and we want to do 'action' for 8 turns.
    # We'll be done when turn 9 is over.
    stop_when = turns + current_turn

    while (current_turn < stop_when):
        yield action;
        current_turn += 1;

commands = {
    "playzero" : playzero,
    "playdec" : playdec,
    "playsucc" : playsucc,
    "playdbl" : playdbl,
}

def strategy(cmds, turn):
    # On any given turn, we're only going to use one of the commands
    # in cmds.  Pick the right one out of the list based on which
    # of our turns it is.
    splitted = cmds.split()
    i = turn % len(splitted)

    # Then pick the corresponding function out of the commands dict.
    funk = commands[splitted[i]]

    # And run it.
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

            if turn <= 1:
                # Set up slot 0 with 1.
                strategy("playzero playsucc", turn)

            elif turn <= 7:
                # Do playdbl 6 times on slot 0.
                gen = get_single_action(6, playdbl, turn)
                f = next(gen)
                f()

                # The above is total overkill (could've just written
                # playdbl() for the same effect), but I'm hoping that
                # eventually we'll be able to use the generator to
                # return the appropriate step of a sophisticated
                # multi-action sequence...

            elif turn == 8:
                # Decrement opponent's slot number (255-64) by one.
                playdec()

            else:

                if turn < 20000:
                    # Decrement opponent's slot 255.
                    strategy("playzero playdec", turn)
                elif turn <= 50000:
                    # Decrement opponent's slot 254.
                    strategy("playzero playsucc playdec", turn)
                else:
                    # Decrement opponent's slot 253.
                    strategy("playzero playsucc playsucc playdec", turn)

            option, card, slot = get_opponent_move()
            turn += 1
        except EOFError as e:
            return

if __name__ == "__main__": main()
