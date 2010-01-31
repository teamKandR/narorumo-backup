#!/usr/bin/python

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

ONE_DAY = 24 * 60 * 60

def timeofday(timestring):
    """Take a time string of the form 2010-02-01T15:45:00.000-05:00 and return
    just hh:mm."""

    assert "T" in timestring
    Tindex = timestring.index("T")
    afterT = timestring[Tindex+1:]
    return afterT[:5]

def usage():
    print ("usage: %s username@gmail.com" % (sys.argv[0]))

def main():
    if len(sys.argv) == 2:
        username = sys.argv[1]
    else:
        usage()
        exit(-1)

    cal_client = gdata.calendar.service.CalendarService()
    query = gdata.calendar.service.CalendarEventQuery(
        user=username,
        visibility='public',
        projection="full")

    query.orderby="starttime"
    query.sortorder="ascending"

    now = time.localtime()
    tomorrow = time.localtime(time.time() + (ONE_DAY))

    start_date=("%4d-%02d-%02d" % (now.tm_year,now.tm_mon,now.tm_mday))
    end_date=("%4d-%02d-%02d" % (tomorrow.tm_year,
                                 tomorrow.tm_mon,
                                 tomorrow.tm_mday))
    print "AGENDA FOR", start_date

    query.start_min = start_date
    query.start_max = end_date 
    feed = cal_client.CalendarQuery(query)

    for event in feed.entry:
        for when in event.when:
            print '%s' % (timeofday(when.start_time),),
            print 'to %s' % (timeofday(when.end_time),),
            print event.title.text
    
if __name__ == "__main__": main()
