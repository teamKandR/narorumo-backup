#!/usr/bin/env python

"""
Pull down today's agenda from Google Calendar and print it to stdout. This only
works with public calendars, but mine is public.

You could change this to use the gdata API to log in, anyway.

Usage goes like this:

$ python agenda.py example.user@gmail.com
"""

__author__ = "Alex Rudnick <alex.rudnick@gmail.com>"

try:
    from xml.etree import ElementTree
except ImportError:
    from elementtree import ElementTree
import warnings
with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    # this throws a warning about a deprecated module; ignore it.
    import gdata.calendar.service

import string
import time
import re
import sys
import os

ONE_DAY = 24 * 60 * 60
ONE_MINUTE = 60

def timeofday(timestring):
    """Take a time string of the form 2010-02-01T15:45:00.000-05:00 and return
    just hh:mm."""

    assert "T" in timestring
    Tindex = timestring.index("T")
    afterT = timestring[Tindex+1:]
    return afterT[:5]

def dayof(timestring):
    """Take a time string of the form 2010-02-01T15:45:00.000-05:00 and return
    just the date as YYYY-mm-dd."""

    if "T" in timestring:
        Tindex = timestring.index("T")
        return timestring[:Tindex]
    else:
        return timestring

def usage():
    print ("usage: %s username@gmail.com [\"timezone\"]" % (sys.argv[0]))

def get_agenda(username, ctz):
    now = time.localtime()
    tomorrowtime = time.localtime(time.time() + ONE_DAY)

    today = ("%4d-%02d-%02d" % (now.tm_year,now.tm_mon,now.tm_mday))
    tomorrow = ("%4d-%02d-%02d" % (tomorrowtime.tm_year,
                                 tomorrowtime.tm_mon,
                                 tomorrowtime.tm_mday))

    cal_client = gdata.calendar.service.CalendarService()
    query = gdata.calendar.service.CalendarEventQuery(
        user=username,
        visibility='public',
        projection="full")

    query.orderby="starttime"
    query.sortorder="ascending"
    query.ctz = ctz

    query.start_min = today
    query.start_max = tomorrow
    feed = cal_client.CalendarQuery(query)
    
    events = [event for event in feed.entry]
    thekey = lambda ev: ev.when[0].start_time
    events.sort(key=thekey)

    out = ""
    out += ("AGENDA FOR " + today + "\n")
    for event in events:
        for when in event.when:
            if today not in (dayof(when.start_time), dayof(when.end_time)):
                continue

            if "T" in when.start_time:
                out += '%s ' % (timeofday(when.start_time),)
                out += 'to %s ' % (timeofday(when.end_time),)
            else:
                out += "today" + (" " * 10)
            out += event.title.text + "\n"
    return out

CACHEFN = os.path.expanduser("~/.agenda")
def save_agenda(agenda):
    with open(CACHEFN, "w") as outfile:
        outfile.write(agenda)

def retrieve_agenda():
    try:
        with open(CACHEFN, "r") as infile:
            return infile.read()
    except:
        return "(no cached agenda)"

import sys
def main():
    try:
        if len(sys.argv) == 3:
            ctz = sys.argv[2]
        else:
            ctz = "America/New_York"

        if len(sys.argv) in (2,3):
            username = sys.argv[1]
        else:
            usage()
            exit(-1)
        agenda = get_agenda(username, ctz)
        save_agenda(agenda)
        print(agenda)
    except Exception,e:
        print(e) 
        print("[cached]")
        print(retrieve_agenda())
 
if __name__ == "__main__": main()
