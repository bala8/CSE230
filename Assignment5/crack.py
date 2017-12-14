
from misc import *
import crypt
import re

def load_words(filename,regexp):
    """Load the words from the file filename that match the regular
       expression regexp.  Returns a list of matching words in the order
       they are in the file."""
    res = []
    with open(filename, "r") as f:
      for line in f.readlines():
        temp = line.strip()
        if re.match(regexp, temp):
          res.append(temp)
    return res

def transform_reverse(str):
    """Return a list with string passed and reverse as string"""
    return [str, str[::-1]]

def transform_capitalize(str):
    """Returns a list of all the possible ways to capitalize the input string.
       I'm using bit value to produce either upper case or lower case character"""
    str = str.lower()
    res = []
    n = 1 << len(str)
    for i in range(n):
      temp = ""
      for j in range(len(str)):
        if (i >> j) & 1 == 1:
          temp += str[j].upper()
        else:
          temp += str[j]
      res.append(temp)
    return res

def transform_digits(str):
    """return return a list of all possible ways to replace letters with similar 
    looking digits according to the provided mappings"""
    t_map = dict()
    t_map['o'] = ['0']
    t_map['i'] = ['1']
    t_map['l'] = ['1']
    t_map['z'] = ['2']
    t_map['e'] = ['3']
    t_map['a'] = ['4']
    t_map['s'] = ['5']
    t_map['b'] = ['6']
    t_map['b'].append('8')
    t_map['t'] = ['7']
    t_map['g'] = ['9']
    t_map['q'] = ['9']
    res = []
    l = [x for x in str]
    temp = []
    for x in l:
      t = [x]
      if x.lower() in t_map:
        for y in t_map[x.lower()]:
          t.append(y)
      temp.append(t)
    res = temp[0]
    for a in temp[1:]:
      res = [x+y for x in res for y in a]
    return res

def check_pass(plain,enc):
    """Check to see if the plaintext plain encrypts to the encrypted
       text enc"""
    return (crypt.crypt(plain, enc[:2]) == enc)

def load_passwd(filename):
    """Load the password file filename and returns a list of
       dictionaries with fields "account", "password", "UID", "GID",
       "GECOS", "directory", and "shell", each mapping to the
       corresponding field of the file."""
    header = ["account", "password", "UID", "GID", "GECOS", "directory", "shell"]
    res = []
    with open(filename, "r") as f:
      for line in f.readlines():
        temp = re.split(":", line)
        i = 0
        for t in temp:
          temp[i] = t.strip()
          i = i + 1
        temp[2] = int(temp[2])
        temp[3] = int(temp[3])
        res.append(dict(zip(header, temp)))
    return res

def crack_pass_file(pass_filename,words_filename,out_filename):
    """Crack as many passwords in file fn_pass as possible using words
       in the file words"""
    data = load_passwd(pass_filename)
    # extracting just words of length 6,7 or 8 
    word_list = load_words(words_filename, r"^[\w!#$%&^?@|~`]{6}$|^[\w!#$%&^?@|~`]{7}$|^[\w!#$%&^?@|~`]{8}$")

    with open(out_filename, 'w+') as f:
      #First going through just the words and their reverse form
      for w in word_list:
        if len(data) == 0:
          break
        for d in data:
          if check_pass(w, d['password']) == True:
            f.write(d['account']+"="+w+"\n")
            f.flush()
            word_list.remove(w)
            data.remove(d)
            break
          elif check_pass(w[::-1], d['password']) == True:
            f.write(d['account']+"="+w[::-1]+"\n")
            f.flush()
            word_list.remove(w)
            data.remove(d)
            break
      
      #going through the string post transformation using digits mapping and its reverse
      for w in word_list:
        if len(data) == 0:
          break
        for d in data:
          for check in transform_digits(w):
            if check_pass(check, d['password']):
              f.write(d['account']+"="+check+"\n")
              f.flush()
              word_list.remove(w)
              data.remove(d)
              break
            elif check_pass(check[::-1], d['password']):
              f.write(d['account']+"="+check[::-1]+"\n")
              f.flush()
              word_list.remove(w)
              data.remove(d)
              break

      # going through the string post capitalization transformation and its reverse
      for w in word_list:
        if len(data) == 0:
          break
        for d in data:
          for check in transform_capitalize(w):
            if check_pass(check, d['password']):
              f.write(d['account']+"="+check+"\n")
              f.flush()
              word_list.remove(w)
              data.remove(d)
              break
            elif check_pass(check[::-1], d['password']):
              f.write(d['account']+"="+check[::-1]+"\n")
              f.flush()
              word_list.remove(w)
              data.remove(d)
              break

      # going through the string post capitalization and digitization transformation and its reverse
      for w in word_list:
        if len(data) == 0:
          break
        for d in data:
          t_temp = transform_capitalize(w)
          for t in t_temp:
            for check in transform_digits(t):
              if check_pass(check, d['password']):
                f.write(d['account']+"="+check+"\n")
                f.flush()
                word_list.remove(w)
                data.remove(d)
                break
              elif check_pass(check[::-1], d['password']):
                f.write(d['account']+"="+check[::-1]+"\n")
                f.flush()
                word_list.remove(w)
                data.remove(d)
                break
