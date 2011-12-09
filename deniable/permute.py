#!/usr/bin/env python

def permute(bitstring, permutation):
    """Given the bitstring and a permutation, return a new bitstring such that
    the bit at position i in the output bitstring came from position
    permutation[i] in the input bitstring."""
    return [bitstring[i] for i in permutation]

def antipermute(bitstring, permutation):
    """Do the inverse of permute."""
    out = [None] * len(bitstring)
    for i in range(len(bitstring)):
        target = permutation[i]
        out[target] = bitstring[i]
    return out

def main():
    print(permute(["a", "b", "c", "d"], [0,1,2,3]))
    print(permute(["a", "b", "c", "d"], [1,2,3,0]))
    print(antipermute(["a", "b", "c", "d"], [1,2,3,0]))

    perm = [1,2,3,0]
    print(
        permute(antipermute(
          antipermute(permute(["a","b","c","d"],perm), perm), perm),
          perm))

if __name__ == "__main__": main()
