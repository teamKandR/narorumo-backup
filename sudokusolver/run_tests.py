#!/usr/bin/env python2.5

from unittest import defaultTestLoader as loader, TextTestRunner as runner
from sys import argv
import tests

if len(argv) > 1:
  suite = loader.loadTestsFromNames(["tests.%s_tests" % s for s in argv[1:]])
else:
  seq = ["tests.%s" % s for s in tests.__all__]
  print seq
  suite = loader.loadTestsFromNames(seq)

runner().run(suite)
