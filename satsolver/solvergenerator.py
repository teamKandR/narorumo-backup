#!/usr/bin/env python3

import string

def getproblem(fn):
    """Returns a tuple containing the number of variables and a list of lines
    of C code. The last line of C code does the conjunction."""

    linenum = 0
    code = []
    with open(fn, "r") as infile:
        nvars = int(infile.readline())

        for line in infile:
            line = line.strip()
            code.append(make_c(line, linenum))
            linenum += 1

        conjunction = " & ".join( "line%d" % (i,) for i in range(linenum))
        code.append("return (%s);" % (conjunction,))

    return(nvars, code)

def make_c(line, linenum):
    """Produces each line of the body of the trysoln function."""
    out = "int line%d = " % (linenum,)

    tokens = line.split()
    chunks = []
    for token in tokens:
        negate = token.startswith("-")
        if negate: token = token[1:]

        chunks.append( "%sgetbit(p, %d)" % ("~" if negate else "",
                                            (int(token) - 1)))
    out += " | ".join(chunks)
    out += ";"
    return out

TEMPLATEFN = "solver.template"
def generatesolver(nvars, generatedcode):
    with open(TEMPLATEFN, "r") as infile:
        template = string.Template(infile.read())
        return template.substitute(NUMVARS=nvars, TRYSOLNLINES=generatedcode)

def produce_c_solver(infn, outfn):
    """Given an input filename, write the C code for the corresponding solver
    into the output filename."""

    nvars,codelines = getproblem(infn)
    trysoln = "  " + ("\n  ".join(codelines))
    solvertext = generatesolver(nvars, trysoln)

    with open(outfn, "w") as outfile:
        outfile.write(solvertext)
