#!/usr/bin/env python

import sys
import struct

TEAM_ID = 27

def pack32(val, out):
  """Write a 32-bit int into the file, as little-endian. File should be an open
  out stream."""
  packed = struct.pack("<I", val)
  out.write(packed)

def packdouble(val, out):
  """Write a 64-bit double into the file, as little-endian. File should be an
  open out stream."""
  packed = struct.pack("<d", val)
  out.write(packed)

def write_headers(scenario, out):
  """Put the appropriate headers into the file. The scenario should be passed
  as a number."""
  pack32(0xCAFEBABE, out)
  pack32(TEAM_ID, out)
  pack32(scenario, out)

def readframes(trace):
  """trace is an open file. We'll read the lines out one at a time and make
  trace tuples out of them, which look like (ts, [(port,val)... ])."""

  linenum = 0
  out = []

  while True:
    line = trace.readline()
    if line == "": break
    (timestep, inputcount) = tuple(map(int, line.split()))

    inputs = []

    for i in range(inputcount):
      inputline = trace.readline()
      splitted = inputline.split()
      portandval = (int(splitted[0]), float(splitted[1]))
      inputs.append(portandval)
    out.append((timestep, inputs))
  return out

def tuples_to_dict(tuples):
  out = {}
  for tup in tuples:
    out[tup[0]] = tup[1]
  return out

def dict_to_tuples(dict):
  return [(key, dict[key]) for key in dict.iterkeys()]

def dict_of_changes(olddict, newdict):
  out = {}
  for key in newdict.iterkeys():
    if (not key in olddict) or olddict[key] != newdict[key]:
      out[key] = newdict[key]
  return out

def write_frames(frames, out):
  prev_inputs = {}

  for frame in frames:
    inputs = tuples_to_dict(frame[1])
    changed_dict = dict_of_changes(prev_inputs, inputs)
    changed_inputpairs = dict_to_tuples(changed_dict)

    # If something changed, write it.
    if changed_inputpairs:
      write_frame(frame[0], changed_inputpairs, out)

    prev_inputs = inputs

def write_frame(timestep, inputs, out):
  # Frame headers contain the timestep, followed by the number of inputs.
  print "frame at timestep", timestep
  pack32(timestep, out)

  print "has this many inputs:", len(inputs)
  pack32(len(inputs), out)

  # Then port/value pairs.
  for input in inputs:
    port,value = input
    print "port",port,"val",value
    pack32(port, out)
    packdouble(value, out)

def write_final_frame(timestep, out):
  write_frame(timestep, [], out)

def main():
  if len(sys.argv) < 4:
    print ("usage %s scenarionum input out" % sys.argv[0])
    exit(1)
  try:
    scenario = int(sys.argv[1])
    trace = open(sys.argv[2], "r")
    out = open(sys.argv[3], "wb")

    frames = readframes(trace)
    write_headers(scenario, out)
    write_frames(frames, out)

    # We assume that the very last frame in frames is the one where we got the
    # score.
    SCORE_AT_STEP = len(frames)

    # At the timestep immediately after we get a score, write the end frame.
    write_final_frame(SCORE_AT_STEP, out)

  finally:
    trace.close()
    out.close()

if __name__ == "__main__":
  main()
