#!/l/python2.6/bin/python

"""
templates.py: code for doing simple HTML templating.

Alex Rudnick and Dustin Dannenhauer, Spring 2010
"""

import string

def loadtemplate(name):
    """Create and return a string template object from the file
    templates/<name>.template"""

    try:
        with open("templates/" + name + ".template", "r") as infile:
            bytes = infile.read()
            out = string.Template(bytes)
            return out
    except:
        return string.Template("(template didn't load)")

def printheader(loggedin):
    template = loadtemplate("header")
    print template.substitute(LOGGEDIN=loggedin)

def printfooter(loggedin):
    template = loadtemplate("footer")
    print template.substitute(LOGGEDIN=loggedin)
