#!/usr/bin/env python

import cgi
import cgitb
import os
import sqlite3

import templates
import snippetutils
import db

loggedin = snippetutils.get_logged_in_user()

def newsubscribees(text):
    """Return the list of new subscribees specified in the text."""
    out = []
    onwhitespace = text.split()
    for token in onwhitespace:
        splitted = token.split(",")
        for tok in splitted:
            if snippetutils.valid_username(tok):
                out.append(tok)
    return out

data = cgi.FieldStorage();
literaltext = ""
if (('REQUEST_METHOD' in os.environ)
    and (os.environ["REQUEST_METHOD"] == "POST")):
    if data.has_key("text"):
        literaltext = data["text"].value

newset = set(newsubscribees(literaltext))
current = set(db.getsubscriptions(loggedin))

to_add = list(newset - current)
for username in to_add:
    db.savesubscription(loggedin, username)

to_remove = list(current - newset)
for username in to_remove:
    db.removesubscription(loggedin, username)

print "Content-type: text/html\n"
templates.printheader(loggedin)
print "<p>OK!</p>"

if to_add: print "<p>added: ", snippetutils.listout(to_add) ,"</p>"
if to_remove: print "<p>removed: ", snippetutils.listout(to_remove) ,"</p>"

templates.printfooter(loggedin)
