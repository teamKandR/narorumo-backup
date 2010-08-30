#!/usr/bin/env python

import sqlite3
import datetime

THEDB = "db/snippetdb"

def savesnippet(snippet, user):
    """Save the snippet to the database, for the specified user, with the
    current time."""

    conn = sqlite3.connect(THEDB)
    c = conn.cursor()
    # Create table
    c.execute("""create table if not exists snippets
    (snip text, username text, time timestamp)""")
    
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

def main():
    db = sqlite3.connect(THEDB)
    print db.execute("SELECT * FROM snippets").fetchall()
    db.close()

if __name__ == "__main__": main()
