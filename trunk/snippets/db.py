#!/usr/bin/env python

import sqlite3
import datetime

THEDB = "db/snippetdb"

def savesnippet(snippet, user):
    """Save the snippet to the database, for the specified user, with the
    current time."""

    conn = sqlite3.connect(THEDB)
    c = conn.cursor()
    # Insert a row of data
    c.execute("insert into snippets values (?,?,?)",
        (snippet, user, datetime.datetime.now()))
    
    # Save (commit) the changes
    conn.commit()
    
    # We can also close the cursor if we are done with it
    c.close()


def getallsnippets():
    conn = sqlite3.connect(THEDB)
    conn.row_factory = sqlite3.Row
    c = conn.cursor()
    rows = c.execute("select * from snippets order by time desc").fetchall()
    return rows

def getsnippetsfor(username):
    conn = sqlite3.connect(THEDB)
    conn.row_factory = sqlite3.Row
    c = conn.cursor()
    rows = c.execute("select * from snippets "
                     + "where username = ? "
                     + "order by time desc "
                     , (username,)).fetchall()
    return rows

def getallusers():
    outset = set()
    conn = sqlite3.connect(THEDB)
    conn.row_factory = sqlite3.Row
    c = conn.cursor()
    rows = c.execute("select username from snippets "
                     + "order by username desc").fetchall()
    for row in rows:
        outset.add(row["username"])
    out = list(outset)
    out.sort()
    return out


def savesubscription(subscriber, subscribee):
    conn = sqlite3.connect(THEDB)
    c = conn.cursor()
    c.execute("insert into subscriptions values (?,?)",
        (subscriber, subscribee))
    conn.commit()
    c.close()

def removesubscription(subscriber, subscribee):
    conn = sqlite3.connect(THEDB)
    c = conn.cursor()
    c.execute("delete from subscriptions " +
              "where subscriber = ? and subscribee = ?",
        (subscriber, subscribee))
    conn.commit()
    c.close()

def getsubscriptions(username):
    """Return a list of all the usernames that the given user is subscribed
    to, in alphabetical order."""
    outset = set()
    conn = sqlite3.connect(THEDB)
    conn.row_factory = sqlite3.Row
    c = conn.cursor()
    rows = c.execute("select * from subscriptions "
                     + "where subscriber = ? "
                     , (username,)).fetchall()
    for row in rows:
        outset.add(row["subscribee"])
    out = list(outset)
    out.sort()
    return out

def main():
    """Initialize tables that we'll need if they're not already created and
    print out all the db contents, just to see what we have."""
    conn = sqlite3.connect(THEDB)
    c = conn.cursor()

    # Create tables that we'll need.
    c.execute("""create table if not exists snippets
    (snip text, username text, time timestamp)""")
    c.execute("""create table if not exists subscriptions
    (subscriber text, subscribee text)""")

    conn.commit()
    c.close()

    db = sqlite3.connect(THEDB)
    print db.execute("SELECT * FROM snippets").fetchall()
    print db.execute("SELECT * FROM subscriptions").fetchall()
    db.close()

if __name__ == "__main__": main()
