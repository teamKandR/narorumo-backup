#!/usr/bin/env python3

import sys

# local imports
import streamofbits
import constants

def main():
    if len(sys.argv) != 3:
        print("usage: python3 decryptwithkey.py <infile> <keyfile>")
        return

    inbits = streamofbits.bits(sys.argv[1])
    inkeybits = streamofbits.bits(sys.argv[2])

    cipherbits = list(inbits)
    keybits = list(inkeybits)

    plainbits = []

    encodinglength = constants.ELEMENT_LENGTH * constants.ELEMENTS_PER_BIT
    encodeds = streamofbits.n_at_a_time(cipherbits, encodinglength)
    for encoded,keybit in zip(encodeds, keybits):
        nextbit = encoded[0] ^ keybit
        plainbits.append(nextbit)
    print(bytes.decode(bytes(streamofbits.bits_to_bytes(plainbits))), end="")

if __name__ == "__main__": main()
