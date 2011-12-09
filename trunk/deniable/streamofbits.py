#!/usr/bin/env python3

import sys

def eightbits(b):
    assert (0 <= b <= 255)
    return [(b & (1 << shift)) >> shift
            for shift in range(7, -1, -1)]
    
def bits(fn):
    """Returns a generator that gives you the bits in the specified filename,
    one at a time."""
    with open(fn, "rb") as infile:
        while True:
            nextchar = infile.read(1)
            if nextchar == b"": return
            for bit in eightbits(ord(nextchar)):
                yield(bit)

def n_at_a_time(things, n):
    pos = 0
    while pos < len(things):
        yield things[pos:pos+n]
        pos += n

def bits_to_bytes(bits):
    assert (len(bits) % 8 == 0)
    out = []
    for chunk in n_at_a_time(bits, 8):
        assert(len(chunk) == 8)
        val = 0
        for i in range(8):
            val <<= 1
            val += chunk[i]
        out.append(val)
    return out

def main():
    allthebits = list(bits(sys.argv[1]))
    print(len(allthebits))
    print(eightbits(1))
    print(eightbits(2))
    print(eightbits(7))
    print(eightbits(255))

if __name__ == "__main__": main()
