#!/usr/bin/env python3

def scalarproduct(x1, x2):
    """Given the two lists of bits x1 and x2, compute their scalar product in
    mod-2 space. Returns an int, 0 or 1."""
    assert(len(x1) == len(x2))
    out = 0
    for left, right in zip(x1,x2):
        out += (left * right)
        out = out % 2
    return out

def main():
    print(scalarproduct([1,1,1,1], [1,1,1,1]))
    print(scalarproduct([1,1,1,1,1], [1,1,1,1,1]))
    print(scalarproduct([1,0,1,1], [1,1,0,1]))

if __name__ == "__main__": main()
