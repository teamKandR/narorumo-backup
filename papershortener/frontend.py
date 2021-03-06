#!/usr/bin/env python

from google.appengine.ext import webapp
from google.appengine.ext.webapp import util
from google.appengine.ext.webapp import template

import papershortener
import os

class MainHandler(webapp.RequestHandler):
    def get(self):
        path = os.path.join(os.path.dirname(__file__), 'static/index.html')
        template_values = {}
        self.response.out.write(template.render(path, template_values))

    def post(self):
        text = self.request.get("text")
        shorter = papershortener.shorten(text)
        template_values = {"output": shorter}

        if self.request.get("plaintext"):
            self.response.headers['Content-Type'] = 'text/plain'
            self.response.out.write(shorter)
        else:
            path = os.path.join(os.path.dirname(__file__),
                                'templates/output.html')
            self.response.out.write(template.render(path, template_values))

def main():
    application = webapp.WSGIApplication([('.*', MainHandler)],
                                           debug=False)
    util.run_wsgi_app(application)

if __name__ == '__main__':
    main()
