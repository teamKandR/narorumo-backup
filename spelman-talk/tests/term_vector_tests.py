from unittest import TestCase

from build_term_vector import countwords

words = "the quick brown fox jumps over the lazy sleeping dog".split()

class TermVectorTests(TestCase):

  def testCountWords(self):
    counts = countwords(words)

    assert counts['the'] == 2
    assert counts['fox'] == 1
    assert counts['mango'] == 0
