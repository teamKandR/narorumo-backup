from treebank import *

__all__ = ['word_tokenize', ]

# Standard word tokenizer.
_word_tokenize = TreebankWordTokenizer().tokenize
def word_tokenize(text):
    """
    Use NLTK's currently recommended word tokenizer to tokenize words
    in the given sentence.  Currently, this uses
    L{TreebankWordTokenizer}.  This tokenizer should be fed a single
    sentence at a time.
    """
    return _word_tokenize(text)
