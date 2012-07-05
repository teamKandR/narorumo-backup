#!/usr/bin/env python3

"""Send an email to somebody exhorting them to read a paper.

The paper is specified by a URL, read one at a time out of the "tosend" file.
That URL is then appended to the "sent" file, and removed from the beginning of
the "tosend" file. (so the tosend file is a queue of papers to read.)
"""

import argparse
import os
import smtplib
from email.mime.text import MIMEText

def get_parameters():
    parser = argparse.ArgumentParser()
    parser.add_argument("email", type=str,
                        help="email address where we send the email")
    parser.add_argument("--tosend", type=str,
                        help="file where we keep URLs to send, one per line",
                        required=True,
                        )
    parser.add_argument("--sent", type=str,
                        help="file where we keep URLs we have sent, one per line",
                        required=True,
                        )
    parser.add_argument("--message", type=str,
                        help="file where containing message to email",
                        required=False,
                        )
    return parser.parse_args()

def get_first_line(fn):
    assert os.path.exists(fn)
    with open(fn, "r") as infile:
        line = infile.readline()
        return line.strip()

def remove_first_line(fn):
    assert os.path.exists(fn)
    after_first_line = ""
    with open(fn, "r") as infile:
        infile.readline()
        after_first_line = infile.read()
    with open(fn, "w") as outfile:
        print(after_first_line, file=outfile, end="")

def append_line(line, fn):
    with open(fn, "a") as outfile:
        print(line, file=outfile)

def buildmessage(url, message, email_address):
    text = message
    text += url
    msg = MIMEText(text)
    msg['Subject'] = 'read this paper!'
    msg['From'] = "paperaday-noreply@cs.indiana.edu"
    msg['To'] = email_address
    return msg

def sendemail(url, message_fn, email_address):
    """Send an email to email_address."""
    msg = buildmessage(url, message_fn, email_address)
    s = smtplib.SMTP("localhost")
    s.sendmail("paperaday-noreply@cs.indiana.edu",
               [email_address],
               msg.as_string())
    s.quit()

def main():
    args = get_parameters()
    url = get_first_line(args.tosend)
    if not url:
        url = "NO URLS LEFT IN THE FILE PLEASE ADD MORE"
    remove_first_line(args.tosend)
    append_line(url, args.sent)

    message = ""
    if args.message:
        assert os.path.exists(args.message)
        with open(args.message) as infile:
            message = infile.read()
    sendemail(url, message, args.email)

if __name__ == "__main__": main()
