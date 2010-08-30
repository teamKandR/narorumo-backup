#!/usr/bin/env python

import cgi
import cgitb
import os
import sqlite3

import templates
import snippetutils
import db

loggedin = snippetutils.get_logged_in_user()

data = cgi.FieldStorage();
if data.has_key("subscribee"):
    subscribee = data["subscribee"].value

    current = set(db.getsubscriptions(loggedin))
    if snippetutils.valid_username(subscribee) and subscribee not in current:
        db.savesubscription(loggedin, subscribee)

print "Content-type: text/html\n"
templates.printheader(loggedin)

if not snippetutils.valid_username(subscribee):
    print "<p>invalid username?</p>"
else:
    print "<p>OK!</p>"

templates.printfooter(loggedin)
