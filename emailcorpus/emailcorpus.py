#!/usr/bin/env python3

import getpass, imaplib

import imaplib

LABEL = "blast"

def main():
    username = input("username: ")
    try:
        imap_server = imaplib.IMAP4_SSL("imap.gmail.com",993)
        imap_server.login(username, getpass.getpass())
        imap_server.select(LABEL)
        typ, data = imap_server.search(None, 'ALL')
        with open("blastcorpus.txt", "w+") as outfile:
            for num in data[0].split():
                typ, data = imap_server.fetch(num, '(RFC822)')
                # ignore, response = imap_server.fetch(num,
                #     '(body[header.fields (subject)])')
                # print(response[0][1][9:])
                # print('Message %s\n%s\n' % (num, data[0][1]))
                print("--- message ---", file=outfile)
                print(data[0][1], file=outfile)
    finally:
        imap_server.close()
        imap_server.logout()

if __name__ == "__main__": main()
