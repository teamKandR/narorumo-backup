#!/usr/bin/env python2.5

from Board import Board
import unittest

class TestBoard(unittest.TestCase):
  def testInit(self):
    self.assertEquals("foo", "foo")
    self.assertEquals("foo", "bar")
