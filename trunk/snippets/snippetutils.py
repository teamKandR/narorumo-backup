import re
import os

## linebreaks simplified from Django's awesome html filters.
## http://code.djangoproject.com/browser/django/trunk/django/utils/html.py
def linebreaks(value):
    """Converts newlines into <p> and <br />s."""
    # normalize newlines
    value = re.sub(r'\r\n|\r|\n', '\n', unicode(value))
    paras = re.split('\n{2,}', value)
    paras = [u'<p>%s</p>' % p.replace('\n', '<br />') for p in paras]
    return u'\n\n'.join(paras)


def get_logged_in_user():
    ## get username: by default it's the logged-in person.
    if "REMOTE_USER" in os.environ:
        username = str(os.environ["REMOTE_USER"]).split('@')[0]
    else:
        username = "nobody"
    return username

def listout(usernames):
    return ", ".join(usernames)

username_pat = re.compile(r'^[a-z][a-z0-9]{2,7}$')
def valid_username(username):
    return username_pat.match(username) is not None
