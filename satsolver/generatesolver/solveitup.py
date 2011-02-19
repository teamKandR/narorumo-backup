#!/usr/bin/env python3

from solvergenerator import produce_c_solver

import sys
import subprocess
import os

def main():
    if(len(sys.argv) != 2):
        print("usage: python3 solveitup.py <problem description>")
        return

    produce_c_solver(sys.argv[1], "generated.c")
    subprocess.call(
        "gcc -std=gnu99 -pedantic -Wall -O3 generated.c -o generated".split())
    out = subprocess.getoutput("./generated")
    print(str(out))

if __name__ == "__main__": main()
