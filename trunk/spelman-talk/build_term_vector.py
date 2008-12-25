#!/usr/bin/env python2.5

import sys
import copy
import re
import os
import math

TOTAL = "*TOTAL*"
ndocs = 0

class DefaultDict(dict):
  """Dictionary with a default value for unknown keys."""
  def __init__(self, default):
    self.default = default

  def __getitem__(self, key):
    if key in self: 
      return self.get(key)
    else:
      ## Need copy in case self.default is something like []
      return self.setdefault(key, copy.deepcopy(self.default))

  def __copy__(self):
    copy = DefaultDict(self.default)
    copy.update(self)
    return copy

def build_stopword_set():
  out = set()
  f = open("stopwords_en")
  lines = f.readlines()
  for line in lines:
    out.add(line.strip())
  return out

def most_common_words(counts, k):
  """Takes the output of countwords and returns the k most common words."""
  pairs = counts.items()
  pairs.sort(key=lambda(x):x[1], reverse=True)

  return pairs[1:1+k]

def top_tf_idfs(words, doc_frequencies, k):
  """Takes the output of countwords and returns the k top words by tf-idf."""

  docwords = words.keys()

  words_to_scores = {}
  for word in docwords:
    score = tf_idf(word, words, doc_frequencies)
    words_to_scores[word] = score

  pairs = words_to_scores.items()
  pairs.sort(key=lambda(x):x[1], reverse=True)

  return pairs[1:1+k]

def tf_idf(word, words, doc_frequencies):
  """Returns the tf-idf for a given word in a document and corpus. word is a
  string, words comes from countwords, doc_frequencies is the document
  frequencies table."""

  if word == TOTAL:
    return 0.0

  tfscore = tf(word, words)
  idfscore = idf(word, doc_frequencies)

  return tfscore * idfscore

def tf(word, words):
  """Calculates the term-frequency for a given word in a document. word is a
  string, words is the output of countwords."""
  return words[word] / (1.0 * words[TOTAL])

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
    countmap = DefaultDict(0)
    countmap[TOTAL] = 0
  for word in words:
    countmap[word] += 1
    countmap[TOTAL] += 1
  return countmap

def document_frequencies():
  global ndocs

  """For doing tf-idf, count how many documents have each word."""
  out = DefaultDict(0)
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

def listwords(fn):
  """List all the words stored in the specified file."""
  f = open(fn)
  str = f.read()
  splitted = str.split()

  # return [word for word in splitted if wordlike.match(word)]
  return [word for word in splitted if wordlike.match(word) and not is_stopword(word)] 

def is_stopword(word):
  """True if the specified word is a stopword"""
  return word in stopwords

wordlike = re.compile("[a-zA-Z]+")
stopwords = build_stopword_set()
frequencies = document_frequencies()

def main():
  d = countwords(listwords(sys.argv[1]))

  # print d
  # print most_common_words(d, 10)
  # print tf("Larry", d)
  # print tf("Google", d)
  # print tf("Sergey", d)
  # print tf("he", d)
  # print tf("the", d)
  # print tf("and", d)

  # print tf("Larry", d)
  # print tf_idf("Larry", d, frequencies)
  print top_tf_idfs(d, frequencies, 10)

if __name__ == "__main__":
  main()

