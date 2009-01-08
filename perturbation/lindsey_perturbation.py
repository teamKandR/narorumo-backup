# For what this is all about, see:
# http://lindseykuper.livejournal.com/265703.html

import re
import string

orig_words = [] 
sorted_to_unsorted = {} 

num_words_with_perturbations = 0
total_perturbations = 0

output = open('perturbations.txt', 'w')

def sort_str(str):
    chars = []
    for char in str:
        chars.append(char)
    chars.sort()
    sorted_str = string.join(chars)
    return sorted_str

for word in open('/usr/share/dict/words'):
    word = word.strip()
    orig_words.append(word)
    sorted_word = sort_str(word)
    if sorted_word not in sorted_to_unsorted:
        sorted_to_unsorted[sorted_word] = [];
    sorted_to_unsorted[sorted_word].append(word);
    
for word in orig_words:
    if re.match('.*m.*', word):
        rb_word = re.sub('m', 'rb', word, 1) # only replace one m!
        sorted_rb_word = sort_str(rb_word)
        if sorted_rb_word in sorted_to_unsorted:
            print >>output, word, "perturbs to:"
            for word in sorted_to_unsorted[sorted_rb_word]:
                print >>output, "\t", word
            num_words_with_perturbations += 1
            total_perturbations += len(sorted_to_unsorted[sorted_rb_word])

print num_words_with_perturbations, "words have perturbations."
print "There are", total_perturbations, "perturbations in all."
