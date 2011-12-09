#!/usr/bin/env python3

import sys

# local imports
import constants
import encrypt
import trapdoor
import streamofbits

def isS(bits, secret):
    """Return True if the bitstring given is an S-element."""
    x0 = bits[:constants.X0LENGTH]
    actualB = bits[constants.X0LENGTH:]
    shouldB = encrypt.calculateB(x0, secret)
    return (actualB == shouldB)

def getElements(longstring):
    """Generator that returns the bits in longstring, ELEMENT_LENGTH bits at a
    time."""
    pos = 0
    length = len(longstring)
    while (pos < length):
        yield longstring[pos:pos+constants.ELEMENT_LENGTH]
        pos += constants.ELEMENT_LENGTH

def countSs(longstring, secret):
    """Count the initial S elements at the front of longstring."""
    out = 0
    for bits in getElements(longstring):
        if isS(bits, secret):
            out += 1
        else:
            break
    return out

def main():
    secret = trapdoor.loadtrapdoor()
    if len(sys.argv) != 2:
        print("usage: python3 decrypt.py <infile>")
        return

    inbits = streamofbits.bits(sys.argv[1])
    cipherbits = list(inbits)
    plainbits = []
    for encoded in streamofbits.n_at_a_time(cipherbits,
                                constants.ELEMENT_LENGTH *
                                constants.ELEMENTS_PER_BIT):
        numS = countSs(encoded, secret)
        plainbits.append(numS % 2)

    print(bytes.decode(bytes(streamofbits.bits_to_bytes(plainbits))), end="")

if __name__ == "__main__": main()
