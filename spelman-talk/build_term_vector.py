#!/usr/bin/env python3

import sys
import copy
import re
import os
import math

from collections import defaultdict

TOTALWORDS = "*TOTAL*"
ndocs = 0

def build_stopword_set():
    out = set()
    f = open("stopwords_en")
    lines = f.readlines()
    for line in lines:
        out.add(line.strip())
    return out

def most_common_words(counts, k):
    """Takes the output of countwords and returns the k most common words."""
    pairs = list(counts.items())
    pairs.sort(key=lambda x:x[1], reverse=True)
    return pairs[1:1+k]

def top_tf_idfs(words, doc_frequencies, k):
    """Takes the output of countwords and returns the k top words by tf-idf."""

    docwords = list(words.keys())

    words_to_scores = {}
    for word in docwords:
        score = tf_idf(word, words, doc_frequencies)
        words_to_scores[word] = score

    pairs = list(words_to_scores.items())
    pairs.sort(key=lambda x:x[1], reverse=True)
    return pairs[1:1+k]

def tf_idf(word, words, doc_frequencies):
    """Returns the tf-idf for a given word in a document and corpus. word is a
    string, words comes from countwords, doc_frequencies is the document
    frequencies table."""

    if word == TOTALWORDS:
        return 0.0

    tfscore = tf(word, words)
    idfscore = idf(word, doc_frequencies)

    return tfscore * idfscore

def tf(word, words):
    """Calculates the term-frequency for a given word in a document. word is a
    string, words is the output of countwords."""
    return words[word] / (1.0 * words[TOTALWORDS])

def idf(word, doc_frequencies):
    """The log of 1/ P(word appears in a document in the corpus)"""
    if word in doc_frequencies:
        return math.log( ndocs / doc_frequencies[word] )
    else:
        raise Exception("haven't seen that word yet:" + word)

def countwords(words, countmap=None):
    """Takes a list of words and returns a map from words to the number of times
    that word occurs. If countmap is specified, add the words into that, else
    build a new one."""

    if not countmap:
        countmap = defaultdict(lambda: 0)
        countmap[TOTALWORDS] = 0
    for word in words:
        countmap[word] += 1
        countmap[TOTALWORDS] += 1
    return countmap

def document_frequencies():
    global ndocs

    """For doing tf-idf, count how many documents have each word."""
    out = defaultdict(lambda: 0)
    for root, dirs, fileshere in os.walk("documents"):
        for fn in fileshere:
            if fn.endswith(".txt"):
                ndocs += 1
                whole_pathname = os.path.join(root, fn)
                words = listwords(whole_pathname)
                wordsonce = set(words)
                for word in wordsonce:
                  out[word] += 1
    return out

wordlike = re.compile("[a-zA-Z]+")
def listwords(fn):
    """List all the words stored in the specified file."""
    f = open(fn)
    text = f.read()
    splitted = text.split()
    return [word.lower() for word in splitted
                         if wordlike.match(word) and not is_stopword(word)]

stopwords = build_stopword_set()
def is_stopword(word):
    """True if the specified word is a stopword"""
    return word in stopwords

def main():
    if len(sys.argv) > 1: fn = sys.argv[1]
    else: fn = "documents/Andorra.txt"

    d = countwords(listwords(fn))
    frequencies = document_frequencies()

    print(top_tf_idfs(d, frequencies, 10))

if __name__ == "__main__": main()
