#!/usr/bin/env python

import sys
import os
import nltk
import string
import re

def allwords(ngram):
    """
    Returns True iff all the tokens in the sequence are strings that contain
    only letters.
    >>> allwords(("foo","bar","baz"))
    True
    >>> allwords(("foo",".","baz"))
    False
    >>> allwords((None,"baz"))
    False
    """
    for token in ngram:
        if not isinstance(token, basestring):
           return False 
        for c in token:
            if c not in string.letters: return False
    return True

def make_acronym(tokens):
    """Take a list of tokens and produce an acronym consisting of their first
    letters.
    """
    return "".join( [token[0] for token in tokens] ).upper() 

## so here's the plan.
# - segment the text into sentences
# - now for each sentence, find all the ngrams in it. Add those acronyms to the
# big list of acronyms

# Now: for each acronym we've found:
# - build a regular expression that matches the words in that acronym
# - iterate over the matches.
# - on the second match and subsequent matches, output everything before that
# second match and the acronym. Once you're out of matches, output the rest of
# the text.
# The input for subsequent loops is the output from previous loops.

MARKER = "!@#$"
MARKERNL = "^&*()"

def acronym_replace(text, tokens, acronym):
    """Replace the specified sequence of tokens (given as a tuple) with the
    specified acronym. The first instance has the acronym appended to it; all
    subsequent instances are just replaced with the acronym."""

    textchunks = []
    pattern = r'\b' + r'\s'.join(tokens) + r'\b'
    compiled = re.compile(pattern)

    matchiter = compiled.finditer(text)
    try:
        matchiter.next()
        matchiter.next()
    except StopIteration:
        return text

    matchiter = compiled.finditer(text)
    first = True
    last_match_end = 0

    for match in matchiter:
        start,end = match.start(), match.end()
        textchunks.append(text[last_match_end:start])

        if first:
            textchunks.append(text[start:end])
                #text[start:end].replace(" ", MARKER).replace("\n", MARKERNL) )
            textchunks.append(" (" + acronym + ")")
            first = False
        else:
            textchunks.append(acronym)
        last_match_end = end

    textchunks.append(text[last_match_end:])
    return "".join(textchunks)

# each ngram we add to the table gets its own unique serial number. These will
# be used for sorting later; ngrams that appear earlier get anagrammed earlier.
serialnum = 0

def add_acronyms(tokens, table):
    """Given a list of tokens, add the set of possible acronyms (for each
    unique ngram of length >1 in that list) into the given acronym table."""

    global serialnum
    for n in xrange(2, len(tokens) + 1):
        ngrams = nltk.util.ngrams(tokens, n)
        for ngram in ngrams:
            if allwords(ngram):
                if ngram in table:
                    acronym,count,serial = table[ngram]
                    table[ngram] = (acronym, count+1, serial)
                else:
                    table[ngram] = (make_acronym(ngram), 1, serialnum)
                    serialnum += 1

def cmpkeypair(pair1, pair2):
    if len(pair1[0]) < len(pair2[0]):
        return -1
    if len(pair1[0]) > len(pair2[0]):
        return 1
    if pair1[1] < pair2[1]:
        return -1
    if pair1[1] > pair2[1]:
        return 1
    return 0


## duty now for the future: be callable from a web frontend.

INITIALIZED = False
sent_segmenter = None

def init():
    global sent_segmenter
    global INITIALIZED

    if INITIALIZED: return
    
    path = os.path.join(os.path.dirname(__file__), 'english.pickle')
    sent_segmenter=nltk.data.load('file:' + path)
    INITIALIZED = True

def shorten(text):
    global sent_segmenter
    init()

    text = text.replace("\r", "")
    sents = sent_segmenter.tokenize(text)

    acronym_table = {}
    for sent in sents:
        tokens = nltk.word_tokenize(sent)
        add_acronyms(tokens, acronym_table)

    # Entries in the acronym_table are keyed by individual ngrams; the values
    # are tuples of (acronym, count, serialnumber).
    keypairs = [ (k,v[2]) for k,v in acronym_table.iteritems() ]
    keypairs.sort(cmp=cmpkeypair, reverse=True)

    for keypair in keypairs:
        acronym,count,serial = acronym_table[keypair[0]]
        if count >= 2:
            text = acronym_replace(text, keypair[0], acronym)
    return text

def main():
    if len(sys.argv) < 2:
        print "file to open?"
        return
    fn = sys.argv[1]
    raw = open(fn).read()

    print shorten(raw)

def test():
    import doctest
    doctest.testmod()

if __name__ == "__main__": main()
