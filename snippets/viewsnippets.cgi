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

## ... but if it's passed as a query parameter, then use that instead.
data = cgi.FieldStorage();
if data.has_key("username"):
    username = data["username"].value
else:
    username = loggedin

rows = []
if snippetutils.valid_username(username):
    rows = db.getsnippetsfor(username)
else:
    username = None

print "Content-type: text/html\n"
templates.printheader(loggedin)

if username is not None and username != loggedin:
    template = templates.loadtemplate("subscribeto")
    print template.substitute(USERNAME=username)

template = templates.loadtemplate("onesnippet")
if not rows:
    print "<p>No results found?</p>"
for row in rows:
    username = cgi.escape(row["username"])
    snip = snippetutils.linebreaks(cgi.escape(row["snip"]))
    time = row["time"]
    print template.substitute(USERNAME=username, TIMESTAMP=time, SNIP=snip)
templates.printfooter(loggedin)
