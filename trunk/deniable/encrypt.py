#!/usr/bin/env python3

import random
import sys

# local imports
import trapdoor
import constants
import permute
import streamofbits
from scalarproduct import scalarproduct

def samplex0():
    """Create an x_0 for the purposes of building an S-element."""
    return [random.randint(0,1) for i in range(constants.X0LENGTH)]

def calculateB(x0, secret):
    currentperm = x0
    out = []
    for i in range(constants.BLENGTH):
        currentperm = permute.antipermute(currentperm, secret)
        # print(currentperm)
        b = scalarproduct(currentperm, x0)
        out.append(b)
    return out

def makeS(secret):
    """Return an S-element, given the trapdoor permutation secret."""
    x0 = samplex0()
    B = calculateB(x0, secret)
    return x0 + B

def makeR():
    """Return an R-element."""
    return [random.randint(0,1) for i in range(constants.ELEMENT_LENGTH)]

def encode(b, secret):
    """Given a bit and the trapdoor information, return an encoding of that
    bit. Returns an enormous list of bits, of size ELEMENT_LENGTH *
    ELEMENTS_PER_BIT."""
    assert (b in (0,1))
    out = []

    ## number of S to encode.
    scount = random.sample(range(b, constants.ELEMENTS_PER_BIT, 2), 1)[0]

    if b == 0:
        assert (scount % 2 == 0)
    else:
        assert (scount % 2 == 1)

    for i in range(scount):
        S = makeS(secret)
        out.extend(S)
    for i in range(constants.ELEMENTS_PER_BIT - scount):
        R = makeR()
        out.extend(R)

    assert (len(out) == constants.ELEMENTS_PER_BIT * constants.ELEMENT_LENGTH)
    return out

def main():
    secret = trapdoor.loadtrapdoor()
    if len(sys.argv) < 5 or (sys.argv[-2] not in ("-out", "--out")):
        print("usage: python3 encrypt.py <realtext>",
              "<faketext0> [<faketext1> ...] -out <outfile>")
        return

    faketextfns = sys.argv[2:-2]
    realtextfn = sys.argv[1]
    outfn = sys.argv[-1]

    print("plaintext:", realtextfn)
    print("decoy texts:", faketextfns)
    print("encrypted output:", outfn)

    inbits = streamofbits.bits(realtextfn, infinitepad=True)
    fakestreams = [streamofbits.bits(fn, infinitepad=True)
                   for fn in faketextfns]
    realkey = []
    fakekeys = []

    for fn in faketextfns:
        fakekeys.append([])
    totalbits = 8 * streamofbits.maxfilesize([realtextfn] + faketextfns)

    with open(outfn, "wb") as outfile:
        for bitnum in range(totalbits):
            inbit = next(inbits)
            enc = encode(inbit, secret)
            asbytes = streamofbits.bits_to_bytes(enc)
            outfile.write(bytes(asbytes))
            realkey.append(enc[0] ^ inbit)

            for fakestream,fakekey in zip(fakestreams,fakekeys):
                fakebit = next(fakestream)
                fakekey.append(enc[0] ^ fakebit)

    with open("realkey", "wb") as outfile:
        asbytes = streamofbits.bits_to_bytes(realkey)
        outfile.write(bytes(asbytes))

    for i,fakekey in enumerate(fakekeys):
        with open("fakekey{0}".format(i), "wb") as outfile:
            asbytes = streamofbits.bits_to_bytes(fakekey)
            outfile.write(bytes(asbytes))

if __name__ == "__main__": main()
