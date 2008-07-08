#!/usr/bin/env python

# A perturbation of a string is a permutation, but with one m removed and one r
# and one b added. For example, perturbation is a perturbation of permutation.

# Challenge!: I'll bake cookies for the first person who can generate a list of
# all the words in your words file that have at least one valid perturbation.

def listwords():
  infile = open("/usr/share/dict/words")
  lines = infile.readlines()
  words = [line.strip() for line in lines]

  return words

def wordstotuples(words):
  out = []
  for word in words:
    if 'r' in word and 'b' in word:
      listed = list(word)
      listed.sort()
      out.append( (word, tuple(listed)) )
  return out

def build_tuple_to_words_map(pairs):
  tuple_to_wordlist = {}
  for word,tup in pairs:
    if not tuple_to_wordlist.has_key(tup):
      tuple_to_wordlist[tup] = []
    tuple_to_wordlist[tup].append(word)
  return tuple_to_wordlist

def find_perturbations(words, wordmap):
  for word in words:
    if 'm' in word:
      wordrb = word + "rb"
      aslist = list(wordrb)
      aslist.remove('m')
      aslist.sort()

      tup = tuple(aslist)

      if wordmap.has_key(tup):
        print word + ":", wordmap[tup]

def main():
  words = listwords()
  pairs = wordstotuples(words)
  wordmap = build_tuple_to_words_map(pairs)

  find_perturbations(words, wordmap)

if __name__ == "__main__":
  main()
