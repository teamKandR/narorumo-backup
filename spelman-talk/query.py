#!/usr/bin/env python3

import os
import readline

from build_term_vector import countwords
from build_term_vector import listwords
from build_term_vector import document_frequencies
from build_term_vector import tf_idf
from build_term_vector import TOTALWORDS
from cosine import cosine

def make_tfidf(termvector, doc_frequencies):
    """Takes a term vector of word counts and a table of document frequencies
    and returns a new term vector with each term's tf-idf score."""

    out = {}
    docwords = list(termvector.keys())
    for word in docwords:
        if word == TOTALWORDS: continue
        score = tf_idf(word, termvector, doc_frequencies)
        out[word] = score
    return out

def load_documents(use_tfidf=False,doc_frequencies=None):
    """Build and return a dict from names of documents in the documents
    directories to their term vectors."""
    out = {}
    for root, dirs, fileshere in os.walk("documents"):
        for fn in fileshere:
            if fn.endswith(".txt"):
                whole_pathname = os.path.join(root, fn)
                termvector = countwords(listwords(whole_pathname))
                if use_tfidf:
                    tfidf_vector = make_tfidf(termvector,doc_frequencies)
                    out[whole_pathname] = tfidf_vector
                else:
                    out[whole_pathname] = termvector
    return out

def search(query, documents):
    pairs = [(name,cosine(query, doc)) for name,doc in documents.items()]
    pairs.sort(key=lambda x:x[1], reverse=True)
    return pairs

def main():
    frequencies = document_frequencies()
    # documents = load_documents(use_tfidf=True, doc_frequencies=frequencies)
    documents = load_documents()

    print("press ctrl-D to exit!")
    while True:
        try:
            querytext = input("query> ")
            querytext = querytext.strip().lower()
            if not querytext: continue
            query = countwords(querytext.split())
            for hit in search(query, documents):
                if hit[1]: print(hit)
        except EOFError:
            print()
            break
    print("OK thanks!")

if __name__ == "__main__": main()
