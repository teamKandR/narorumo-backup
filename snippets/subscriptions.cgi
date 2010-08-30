#!/usr/bin/env python

import cgi
import cgitb
import os
import sqlite3

import templates
import snippetutils
import db

loggedin = snippetutils.get_logged_in_user()
usernames = db.getsubscriptions(loggedin)
subscriptions = snippetutils.listout(usernames)

print "Content-type: text/html\n"
templates.printheader(loggedin)
template = templates.loadtemplate("subscriptionbox")
print template.substitute(SUBSCRIPTIONS=subscriptions)

templates.printfooter(loggedin)
