import re

## linebreaks simplified from Django's awesome html filters.
## http://code.djangoproject.com/browser/django/trunk/django/utils/html.py
def linebreaks(value):
    """Converts newlines into <p> and <br />s."""
    # normalize newlines
    value = re.sub(r'\r\n|\r|\n', '\n', unicode(value))
    paras = re.split('\n{2,}', value)
    paras = [u'<p>%s</p>' % p.replace('\n', '<br />') for p in paras]
    return u'\n\n'.join(paras)
