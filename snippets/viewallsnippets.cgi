#!/usr/bin/env python

import cgi
import cgitb
import os
import sqlite3

import templates
import db
import snippetutils

print "Content-type: text/html\n"

loggedin = snippetutils.get_logged_in_user()
templates.printheader(loggedin)

rows = db.getallsnippets()

for row in rows:
    username = cgi.escape(row["username"])
    snip = snippetutils.linebreaks(cgi.escape(row["snip"]))
    time = row["time"]

    template = templates.loadtemplate("onesnippet")
    print template.substitute(USERNAME=username, TIMESTAMP=time, SNIP=snip)

templates.printfooter(loggedin)
