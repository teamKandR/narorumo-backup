#!/usr/bin/env python2.5 
## things I've had to do so far:
## port install python25
## port install py25-sqlite3

"""For Scrabble purposes, we're going to need some datastructure that makes it
dead simple to tell if one bag of letters is a sub-bag of another one."""

import string
import copy
import time
import sqlite3

"""This might end up being a big select. Maybe like "give me the words that
have this many of these letters, or fewer" (for each letter you care about),
and then join all of those sets. Can that be computed quickly?

Each word, in the big Table O' Words, has a field for each letter and a count
of how many of that letter it has.
"""

letters = [chr(letter) for letter in range( ord('a'), ord('z') + 1)]
fields = [ letter + " integer default 0" for letter in letters ]

def commas(lst):
  out = ""
  for thing in lst:
    out += str(thing) + ","
  out = out[:-1]
  return out

def ands(lst):
  out = ""
  for thing in lst:
    out += str(thing) + " and "
  out = out[:-4]
  return out

class WordBag:
  def __init__(self, str):
    if(not str.isalpha()):
      raise ValueError("Not a word?")

    str = str.lower()
    self.counts = [0] * 26
    self.word = str

    a = ord('a')
    for letter in str:
      pos = ord(letter) - a
      self.counts[pos] += 1

  def activeletters(self):
    return [letters[i] for i in range(len(self.counts)) if self.counts[i] > 0]

  def activevalues(self):
    return [count for count in self.counts if count > 0]

  def toINSERT(self):
    fields = self.activeletters()
    values = self.activevalues()

    out = "insert into words (word," + commas(fields) + ")  values (\"" + self.word + "\","  + commas(values) + ")"
    return out

  def save(self, db):
    insert = self.toINSERT();
    c = db.cursor()
    c.execute(insert)
    c.close()

  def __str__(self):
    return str(self.counts)

  def toquery(self, rel="<="):
    together = [ letter + rel + str(count)
      for (letter,count) in zip(letters,self.counts) ]
    
    out = "select * from words where " + ands(together)
    return out

def createdb():
  fn = "words.db"

  cmd = "create table if not exists words (word text, " + commas(fields)
  cmd += ");"

  conn = sqlite3.connect(fn)
  c = conn.cursor()
  c.execute(cmd)
  c.close()
  return conn

def saveeveryword():
  conn = createdb()

  infile = open("/usr/share/dict/words")

  for line in infile.readlines():
    line = line[:-1]
    bag = WordBag(line)
    bag.save(conn)

  conn.commit()
  conn.close()

def printpossiblewords(text, db):
  c = db.cursor()

  bag = WordBag(text)
  query = bag.toquery()

  c.execute(query)

  for row in c:
    print row

def getdb():
  db = sqlite3.connect("words.db")
  return db

class WordsDB:
  def __init__(self):
    self.db = getdb()

  def isaword(self, text):
    c = self.db.cursor()
    query = "select word from words where word=\"%s\"" % text
    # print "QUERY:", query
    c.execute(query)
    
    out = False
    for row in c:
      out = True
    return out

  def possiblewords(self, rack):
    out = []
    c = self.db.cursor()

    bag = WordBag(rack)
    query = bag.toquery()

    c.execute(query)

    for row in c:
      word = str(row[0])
      if len(word) > 1:
        out.append(word)
    return out

def main():
  # createdb()
  # saveeveryword()

  wordsDB = WordsDB()

  while(True):
    print "your rack: ", 
    line = raw_input()
    if(not line): break

    # print isaword(line, db)
    print wordsDB.possiblewords(line)

if "__main__" == __name__:
  main()
