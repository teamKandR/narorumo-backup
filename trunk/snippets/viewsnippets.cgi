#!/usr/bin/env python

import cgi
import cgitb
import os
import sqlite3

import templates
import savesnippet
import snippetutils

print "Content-type: text/html\n"

## get username: by default it's the logged-in person.
loggedin = snippetutils.get_logged_in_user()

## ... but if it's passed as a query parameter, then use that instead.
data = cgi.FieldStorage();
if data.has_key("username"):
    username = data["username"].value
else:
    username = loggedin

conn = sqlite3.connect("db/snippetdb")
conn.row_factory = sqlite3.Row
c = conn.cursor()
rows = c.execute("select * from snippets "
                 + "where username = ? "
                 + "order by time desc "
                 , (username,)).fetchall()

templates.printheader(loggedin)
for row in rows:
    username = cgi.escape(row["username"])
    snip = snippetutils.linebreaks(cgi.escape(row["snip"]))
    time = row["time"]

    template = templates.loadtemplate("onesnippet")
    print template.substitute(USERNAME=username, TIMESTAMP=time, SNIP=snip)
templates.printfooter(loggedin)
