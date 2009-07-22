#!/usr/bin/env python

"""passive.py: First pass at finding passive voice sentences, and more
importantly, getting familiar with NLTK.

Tags a sentence with a way-overkill four-level tagger trained from the Brown
Corpus, and then looks at its verbs. If somewhere in the sentence, there's a
to-be verb and then later on a non-gerund, we'll flag the sentence as probably
passive voice.

Developed against NLTK 2.0b5."""

import nltk
from nltk.corpus import brown

from cPickle import dump, load
import sys
import os
from itertools import dropwhile

TAGGER = None

def create_tagger():
  """Train a tagger from the Brown Corpus. This should not be called very
  often; only in the event that the tagger pickle wasn't found."""
  print "Building tagger..."
  train_sents = brown.tagged_sents()

  # These regexes were lifted from the NLTK book tagger chapter.
  t0 = nltk.RegexpTagger(
    [(r'^-?[0-9]+(.[0-9]+)?$', 'CD'),   # cardinal numbers
     (r'(The|the|A|a|An|an)$', 'AT'),   # articles
     (r'.*able$', 'JJ'),                # adjectives
     (r'.*ness$', 'NN'),                # nouns formed from adjectives
     (r'.*ly$', 'RB'),                  # adverbs
     (r'.*s$', 'NNS'),                  # plural nouns
     (r'.*ing$', 'VBG'),                # gerunds
     (r'.*ed$', 'VBD'),                 # past tense verbs
     (r'.*', 'NN')                      # nouns (default)
    ])
  print "got t0"

  t1 = nltk.UnigramTagger(train_sents, backoff=t0)
  print "got t1"

  t2 = nltk.BigramTagger(train_sents, backoff=t1)
  print "got t2"

  t3 = nltk.TrigramTagger(train_sents, backoff=t2)
  print "Built tagger!"
  return t3

def save_tagger(tagger):
  output = open('tagger.pkl', 'wb')
  dump(tagger, output, -1)
  output.close()

def get_tagger():
  if os.path.exists("tagger.pkl"):
    input = open('tagger.pkl', 'rb')
    tagger = load(input)
    input.close()
    print "The tagger has been loaded from the pickle by Python."
    return tagger
  else:
    tagger = create_tagger()
    save_tagger(tagger)
    return tagger

def tag_sentence(sent):
  """Take a sentence as a string and return a list of (word, tag) tuples."""
  assert isinstance(sent, basestring)

  tokens = nltk.word_tokenize(sent)
  return TAGGER.tag(tokens)

def passivep(tags):
  """Takes a list of tags, returns true if we think this is a passive
  sentence."""
  # Particularly, if we see a "BE" verb followed by some other, non-BE
  # verb, except for a gerund, we deem the sentence to be passive.
  
  postToBe = dropwhile(lambda(tag): not tag.startswith("BE"), tags)
  isVerbNotGerund = lambda(tag): (tag.startswith("V") and
                                  not tag.startswith("VBG"))

  return any(filter(isVerbNotGerund, postToBe))

def oneline(sent):
  """Replace CRs and LFs with spaces."""
  return sent.replace("\n", " ").replace("\r", " ")

def print_if_passive(sent):
  """Given a sentence, tag it and print if we think it's a passive-voice
  formation."""
  tagged = tag_sentence(sent)
  tags = map( lambda(tup): tup[1], tagged)

  if passivep(tags):
    print "passive:", oneline(sent)

punkt = nltk.tokenize.punkt.PunktSentenceTokenizer()
def findpassives(fn):
  with open(fn) as f:
    text = f.read()
    sentences = punkt.tokenize(text)

    for sent in sentences:
      print_if_passive(sent)

def repl():
  """Read eval (for passivity) print loop."""
  try:
    while True:
      line = raw_input()
      print_if_passive(line)
  except EOFError,e:
    pass

def main():
  global TAGGER
  TAGGER = get_tagger()

  if len(sys.argv) > 1:
    for fn in sys.argv[1:]:
      findpassives(fn)
  else:
    repl()

if __name__ == "__main__":
  main()
