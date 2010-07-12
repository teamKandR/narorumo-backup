#!/usr/bin/env python3

from build_term_vector import TOTALWORDS
import math

def dotproduct(query, document):
    total = 0
    for k in query.keys():
        if k == TOTALWORDS: continue
        if k in document:
            total += (query[k] * document[k])
            # print("key in query and document:", query[k], document[k], k)
    return total

def magnitude(d):
    total = 0
    for k,v in d.items():
        if k == TOTALWORDS: continue
        total += (v * v)
    return math.sqrt(total)

def cosine(dict1, dict2):
    """Cosine of two word vectors is their dot product divided by the product
    of their two magnitudes. Is that true?"""

    return dotproduct(dict1, dict2) / (magnitude(dict1) * magnitude(dict2))

def main():
    print(cosine( {"foo":1}, {"foo":1} ))
    print(cosine( {"foo":10}, {"foo":1} ))
    print(cosine( {"foo":1}, {"bar":1} ))

if __name__ == "__main__": main()
