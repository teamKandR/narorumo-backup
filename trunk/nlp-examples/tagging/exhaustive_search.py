#!/usr/bin/env python

"""
Really simple bigram POS tagger for a sentence about time flies and what they
enjoy. I assume they're similar to fruit flies?
"""

__author__ = "Alex Rudnick (alex.rudnick@gmail.com)"


# Dictionary from words to pairs of the form (pos, prob). All prob values here
# are given as percentages, so we'll have to divide by 100. These are the
# probabilities that, given TAG, the word is WORD.
thewordtable = { "time": { "NN": 7.0727,
                           "VB": 0.0005,
                           "JJ": 0.0},
                  "flies": {"VBZ": 0.4754,
                            "NNS": 0.1610},
                  "like": {"IN": 2.6512,
                           "VB": 2.8413,
                           "RB": 0.5086},
                  "an": {"DT": 1.4192},
                  "arrow": {"NN": 0.0215}}

# dictionary from previous tag to a list of (current, prob) pairs. All prob
# values are given as percentages, so divide by 100.
thetagtable = {
  "S": { "NN": 0.6823,
         "VB": 0.5294,
         "JJ": 0.8033 },
  "NN": {"VBZ":3.9005,
         "NNS":1.6076,
         "E":0.2069},
  "VB" : {"VBZ": 0.0566,
          "NNS": 0.6566,
          "DT": 15.2649},
  "JJ" :{"VBZ": 2.0934,
         "NNS": 2.4383},
  "VBZ": {"IN": 8.5862,
          "VB": 0.7002,
          "RB": 15.0350},
  "NNS": {"IN": 21.8302,
          "VB": 11.1406,
          "RB":6.4721},
  "IN" : {"DT": 31.4263},
  "RB" : {"DT": 5.3113}, 
  "DT": {"NN": 38.0170}
}

def allchoices(choices, sofar=[], out=None):
  """choices is a list of lists. Return a list containing all the ways to pick
  one thing from each list."""

  if out == None: out = []
  if choices == []:
    out.append(sofar)
    return out

  for option in choices[0]:
    allchoices(choices[1:], sofar + [option], out)

  return out

def score_tag_sequence(words, tags, wordtable, tagtable):
  """For a given list of words and a list of tags, multiply together all the
  probabilities of the words given the tags and the tags given the previous
  tags."""
  
  prevtag = "S"
  score = 1.0

  for (word, tag) in zip(words, tags):
    # get the probability for the current word, given current tag
    if tag in wordtable[word]:
      wordscore = wordtable[word][tag]
    else:
      wordscore = 0.0

    # get the probability for the current tag, given previous tag
    if tag in tagtable[prevtag]:
      tagscore = tagtable[prevtag][tag]
    else:
      tagscore = 0.0

    score = score * (wordscore / 100.0)
    score = score * (tagscore / 100.0)
    prevtag = tag

  score = score * tagtable[prevtag]["E"]
  return score

def argmax_tag(words, wordtable, tagtable):
    """Given a list of words, a lexical probability table, and a tag transition
    probability table, generate the most likely list of tags for this
    sentence, by just trying all possible combinations ."""

    gettags = lambda(word): possible_tags(word, wordtable)
    tagpossibilities = map(gettags, words)

    sequences = allchoices(tagpossibilities)

    bestscore = 0.0
    bestsequence = None

    for sequence in sequences:
      score = score_tag_sequence(words, sequence, wordtable, tagtable)

      if score > bestscore:
        bestscore = score
        bestsequence = sequence
    return bestsequence

def possible_tags(word, wordtable):
    return wordtable[word].keys()

def main():
    print possible_tags("time", thewordtable)

    words = ["time", "flies", "like", "an", "arrow"]

    # Examples:
    print score_tag_sequence(
      words,
      ["NN", "VBZ", "IN", "DT", "NN"],
      thewordtable, thetagtable)

    print score_tag_sequence(
      words,
      ["VB", "NNS", "VB", "DT", "NN"],
      thewordtable, thetagtable)

    tags = argmax_tag(words, thewordtable, thetagtable)
    print "The best tag sequence is: ", tags

    print "The score for that sequence is: ",
    print score_tag_sequence(words, tags, thewordtable, thetagtable)

if __name__ == "__main__": main()
