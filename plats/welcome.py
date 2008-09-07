import wsgiref.handlers
import os

from google.appengine.ext.webapp import template
from google.appengine.api import users
from google.appengine.ext import webapp

class MainPage(webapp.RequestHandler):
  def get(self):
    template_values = {
      'accept_language': self.request.headers['accept-language']
    }

    path = os.path.join(os.path.dirname(__file__), 'welcome.html')
    self.response.out.write(template.render(path, template_values))

def pick_locale(available, preferred):
  """Returns the language that we'll display in; the most-preferred one
  available, or failing that, just pick one."""

  for lang in preferred:
    if lang in available:
      return lang
  return available[0]

def ordered_languages(accept_language):
  """Take the Accept-Language header string and produce an ordered list of
  language strings, from most preferred to least."""
  langs = accept_language.split(",")
  pairs = map( lambda(lang): lang.split(";"), langs)

  for pair in pairs:
    if len(pair) < 2: pair.append(1)
    else: pair[1] = float(pair[1][2:])
  pairs.sort(key=lambda(x):x[1], reverse=True)

  return [lang for (lang,val) in pairs if val > 0.0]

def main():
  application = webapp.WSGIApplication( [('/', MainPage)], debug=True)
  wsgiref.handlers.CGIHandler().run(application)

if __name__ == "__main__":
  main()
