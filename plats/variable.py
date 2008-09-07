import wsgiref.handlers
import os

from google.appengine.ext.webapp import template
from google.appengine.api import users
from google.appengine.ext import webapp

from plats import pick_locale, ordered_locales

class Variable(webapp.RequestHandler):
  def get(self):
    best = pick_locale(self.request)
    ordered = ordered_locales(self.request.headers['accept-language'])

    template_values = {
      'best': best,
      'ordered': ordered
    }

    self.response.headers.add_header("Content-Type", "application/javascript")

    path = os.path.join(os.path.dirname(__file__), 'put_variable.js')
    self.response.out.write(template.render(path, template_values))

def main():
  application = webapp.WSGIApplication( [('/put_variable.js', Variable)], debug=True)
  wsgiref.handlers.CGIHandler().run(application)

if __name__ == "__main__":
  main()
