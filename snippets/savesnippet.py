#!/usr/bin/env python

import sqlite3
import datetime

def save(snippet, user):
    """Save the snippet to the database, for the specified user, with the
    current time."""

    conn = sqlite3.connect("db/snippetdb")
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

def main():
    db = sqlite3.connect("db/snippetdb")
    print db.execute("SELECT * FROM snippets").fetchall()
    db.close()

if __name__ == "__main__": main()
