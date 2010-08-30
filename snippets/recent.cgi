#!/usr/bin/env python

import cgi
import cgitb
import os
import sqlite3

import templates
import db
import snippetutils

## get username: by default it's the logged-in person.
loggedin = snippetutils.get_logged_in_user()

rows = []
rows = db.getsubscribedsnippets(loggedin)

print "Content-type: text/html\n"
templates.printheader(loggedin)

template = templates.loadtemplate("onesnippet")
if not rows:
    print "<p>No results found?</p>"
for row in rows:
    username = cgi.escape(row["username"])
    snip = snippetutils.linebreaks(cgi.escape(row["snip"]))
    time = row["time"]
    print template.substitute(USERNAME=username, TIMESTAMP=time, SNIP=snip)
templates.printfooter(loggedin)
