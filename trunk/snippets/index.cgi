#!/usr/bin/env python

import cgi
import cgitb
import os
import sqlite3

import templates
import savesnippet
import snippetutils


print "Content-type: text/html\n"

loggedin = snippetutils.get_logged_in_user()
templates.printheader(loggedin)

template = templates.loadtemplate("inputbox")
print template.substitute(LOGGEDIN=loggedin)

templates.printfooter(loggedin)
