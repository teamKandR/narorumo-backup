#!/usr/bin/env python

import cgi
import cgitb
import os

import templates
import savesnippet
import snippetutils

# End of problem class and functions, now on to main stuff
    
data = cgi.FieldStorage();
message = "(didn't get a snippet?)"

# Check if request was a post request
if (('REQUEST_METHOD' in os.environ)
    and (os.environ["REQUEST_METHOD"] == "POST")):
    if data.has_key("text"):
        # message = "o hai"
        escaped = cgi.escape(data["text"].value, quote=True)
        message = snippetutils.linebreaks(escaped)

print "Content-type: text/html\n"

templates.printheader()
template = templates.loadtemplate("upload")
print template.substitute(MESSAGE=message)
templates.printfooter()
