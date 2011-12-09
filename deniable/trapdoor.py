import os 
import random

import constants

def createtrapdoorfile():
    secret = list(range(constants.X0LENGTH))
    random.shuffle(secret)
    with open("TRAPDOOR", "w") as outfile:
        for pos in secret:
            print(pos, file=outfile)

def loadtrapdoor():
    """Load (after possibly creating) the trapdoor file. Returns a permutation
    of the appropriate length."""
    if not os.path.exists("TRAPDOOR"):
        createtrapdoorfile()
    with open("TRAPDOOR") as infile:
        lines = infile.readlines()
        return [int(line.strip()) for line in lines]
