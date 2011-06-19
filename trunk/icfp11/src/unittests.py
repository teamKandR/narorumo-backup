#!/usr/bin/env python3

from ltg_util import gameloop
from ltg_util import build_num_in_slot

def main():
    build_num_in_slot(3, 0)
    build_num_in_slot(40, 1)
    build_num_in_slot(3, 7)
    build_num_in_slot(10, 18)
    build_num_in_slot(0, 255)

    gameloop()

if __name__ == "__main__": main()
