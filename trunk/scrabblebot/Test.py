#!/usr/bin/env python2.5

import unittest
import TestBoard

packages = [TestBoard]

def main():
  suite = unittest.TestSuite()

  for package in packages:
    suite.addTests(unittest.findTestCases(package))

  runner = unittest.TextTestRunner()
  runner.run(suite)

if __name__ == "__main__":
  main()
