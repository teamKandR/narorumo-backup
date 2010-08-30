#!/usr/bin/env python

import cgi
import cgitb
import os
import sqlite3

import templates
import db
import snippetutils

loggedin = snippetutils.get_logged_in_user()

usernames = db.getallusers()

print "Content-type: text/html\n"

templates.printheader(loggedin)
print "<p>All users who have entered snippets:</p>"

template = templates.loadtemplate("userlink")
for username in usernames:
    print template.substitute(USERNAME=username)

templates.printfooter(loggedin)
