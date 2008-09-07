import wsgiref.handlers
import os

from google.appengine.ext.webapp import template
from google.appengine.api import users
from google.appengine.ext import webapp

from plats import pick_locale

class MetaTag(webapp.RequestHandler):
  def get(self):
    best = pick_locale(self.request)

    template_values = {
      'locale': best
    }

    self.response.headers.add_header("Content-Type", "application/javascript")

    path = os.path.join(os.path.dirname(__file__), 'set_metatag.js')
    self.response.out.write(template.render(path, template_values))

def main():
  application = webapp.WSGIApplication( [('/set_metatag.js', MetaTag)], debug=True)
  wsgiref.handlers.CGIHandler().run(application)

if __name__ == "__main__":
  main()
