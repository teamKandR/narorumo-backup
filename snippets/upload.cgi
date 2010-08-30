#!/usr/bin/env python

import cgi
import cgitb
import os

import templates
import db
import snippetutils

loggedin = snippetutils.get_logged_in_user()
data = cgi.FieldStorage();
message = "(didn't get a snippet?)"

# Check if request was a post request
if (('REQUEST_METHOD' in os.environ)
    and (os.environ["REQUEST_METHOD"] == "POST")):
    if data.has_key("text"):
        literaltext = data["text"].value
        escaped = cgi.escape(data["text"].value, quote=True)
        message = snippetutils.linebreaks(escaped)
        db.savesnippet(snippet=literaltext, user=loggedin)

print "Content-type: text/html\n"

templates.printheader(loggedin)
template = templates.loadtemplate("upload")
print template.substitute(MESSAGE=message)
templates.printfooter(loggedin)
