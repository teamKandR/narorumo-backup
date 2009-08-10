#!/usr/bin/env python

import sys
import struct

def topfour(code):
  """Return an int that's the top four bits of the given integer"""
  masked = code & 0xf0000000;
  return (masked >> (32 - 4))

def nextfour(code):
  masked = code & 0x0f000000;
  return (masked >> (32 - 8));

def top_address(code):
  """Returns the address in the slot from bits 27 to 14. Should only be called
  on d-type instructions."""
  top = topfour(code)
  assert (top > 0x0 and top <= 0x6)
  return ((code & 0x0fffffff) >> 14)

def bottom_address(code):
  """Return the address in the bottom 14 bits. Applicable to any type of
  instruction."""

  bottomfourteen = int("1" * 14, 2)
  return (code & bottomfourteen)

def immediate(code):
  """Grab the "immediate" value from an s-type instruction, defined as bits 23
  to 14 inclusive. Kill the top byte (everything up to the s-type instruction
  code) and the bottom 14 bits, which include the r1 register for s-type."""

  assert type(code) == int

  masked = code & 0x00ffffff;
  return (masked >> 14)

def represent_stype(code):
  assert topfour(code) == 0x0

  next = nextfour(code)

  if next == 0x0: out = "NOOP"
  elif next == 0x1: out = "CMPZ"
  elif next == 0x2: out = "SQRT"
  elif next == 0x3: out = "COPY"
  elif next == 0x4: out = "INPUT"
  else:
    print "panic, bad stype!"
    assert False

  imm = immediate(code)

  r1 = bottom_address(code)

  if out == "CMPZ":
    return ("(%s %d %d)" % (out, imm, r1)) 
  else:
    return ("(%s %d)" % (out, r1))

def represent_dtype(code):
  top = topfour(code)

  if top == 0x1: out = "ADD"
  elif top == 0x2: out = "SUB"
  elif top == 0x3: out = "MULT"
  elif top == 0x4: out = "DIV"
  elif top == 0x5: out = "OUTPUT"
  elif top == 0x6: out = "PHI"
  else:
    print "panic, bad dtype!"
    assert False

  r1 = top_address(code)
  r2 = bottom_address(code)

  return ("(%s %d %d)" % (out, r1, r2))

def represent_instruction(instruction):
  top = topfour(instruction)
  next = nextfour(instruction)

  if top == 0x0:
    return represent_stype(instruction)
  else:
    return represent_dtype(instruction)

def bytes_to_int(bytes):
  """Things come to us least-significant first. It's little-endian."""
  assert len(bytes) == 4

  return ( (ord(bytes[0]) << 0) +
           (ord(bytes[1]) << 8) +
           (ord(bytes[2]) << 16) +
           (ord(bytes[3]) << 24))

def read_instruction(bytes, instfirst):
  if instfirst:
    inst_str = bytes[:4]
  else:
    inst_str = bytes[8:]

  instruction = bytes_to_int(inst_str)

  top = topfour(instruction)
  next = nextfour(instruction)
  return represent_instruction(instruction)

def read_data(bytes, instfirst):
  if instfirst:
    data_str = bytes[4:]
  else:
    data_str = bytes[:8]

  # interpret data_str as a 64-bit little-endian floating-point quantity
  return struct.unpack("<d", data_str)[0]

def lispify(lst, name):
  out = "(" + name
  for elt in lst:
    out += " "
    out += str(elt)
  out += ")"
  return out

def main():
  f = open(sys.argv[1])

  instructions = []
  heap = []

  # To start out, it's data first
  instfirst = False

  # we read from f, 12 bytes at a time.
  while True:
    bytes = f.read(12)
    if not bytes:
      f.close()
      break

    instructions.append(read_instruction(bytes, instfirst))
    heap.append(read_data(bytes, instfirst))

    instfirst = not instfirst

  print "(",
  print lispify(instructions, "INSTRUCTIONS")
  print lispify(heap, "HEAP"),
  print ")"

if __name__ == "__main__":
  main()
