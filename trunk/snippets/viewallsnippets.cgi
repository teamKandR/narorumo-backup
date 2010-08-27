#!/usr/bin/env python

import cgi
import cgitb
import os
import sqlite3

import templates
import savesnippet
import snippetutils

print "Content-type: text/html\n"

templates.printheader()

conn = sqlite3.connect("db/snippetdb")
conn.row_factory = sqlite3.Row
c = conn.cursor()
rows = c.execute("select * from snippets").fetchall()

for row in rows:
    username = cgi.escape(row["username"])
    snip = snippetutils.linebreaks(cgi.escape(row["snip"]))
    time = row["time"]

    template = templates.loadtemplate("onesnippet")
    print template.substitute(USERNAME=username, TIMESTAMP=time, SNIP=snip)

templates.printfooter()
