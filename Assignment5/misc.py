#PA 4

import re
import string

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions

def closest_to(l,v):
    """Return the element of the list l closest in value to v.  In the case of
       a tie, the first such element is returned.  If l is empty, None is returned."""
    if not l:
        return None
    else:
        return min(l, key=lambda ele: abs(ele-v))

def make_dict(keys,values):
    """Return a dictionary pairing corresponding keys to values."""
    res = {}
    for i in range(len(keys)):
        res[keys[i]] = values[i]
    return res
   
# file IO functions
def word_count(fn):
    """Open the file fn and return a dictionary mapping words to the number
       of times they occur in the file.  A word is defined as a sequence of
       alphanumeric characters and _.  All spaces and punctuation are ignored.
       Words are returned in lower case"""
    res = {}
    with open(fn) as f:
        for line in f.readlines():
        	temp = re.sub('[%s]' % re.escape('!"#$%&\'()*+,-./:;<=>?@[\\]^`{|}~'), ' ', line.lower())
        	for w in temp.split():
        		if w in res:
        			res[w] += 1
        		else:
        			res[w] = 1
    return res

