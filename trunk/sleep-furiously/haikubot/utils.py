#!/usr/bin/env python3

def levenshtein(a, b):
    """Given two strings a and b, return the edit distance from a to b, where
    possible edit operations are insert, delete, or substitute."""

    lenA, lenB = len(a), len(b)

    table = [[0 for i in range(lenB + 1)] for j in range(lenA + 1)]

    for i in range(0, lenA + 1):
        table[i][0] = i # deletion
    for j in range(0, lenB + 1):
        table[0][j] = j # insertion
    for j in range(1, lenB + 1):
        for i in range(1, lenA + 1):
            if a[i-1] == b[j-1]:
                table[i][j] = table[i-1][j-1]
            else:
                table[i][j] = min(table[i-1][j] + 1,
                                  table[i][j-1] + 1,
                                  table[i-1][j-1] + 1)
    return table[lenA][lenB]
