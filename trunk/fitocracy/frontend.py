#!/usr/bin/env python

from google.appengine.ext import webapp
from google.appengine.ext.webapp import util
from google.appengine.ext.webapp import template

import os

import fitocracy

class MainHandler(webapp.RequestHandler):
    def get(self):
        path = os.path.join(os.path.dirname(__file__), 'static/index.html')
        template_values = {}
        self.response.out.write(template.render(path, template_values))

    def post(self):
        text = self.request.get("text")

        total_miles = fitocracy.total_miles_from_text(text)

        if self.request.get("kilometers"):
            distancetype = "kilometers"
            total_kilometers = 1.609344 * total_miles
            output_distance = ("%0.2f" % total_kilometers)
        else:
            distancetype = "miles"
            output_distance = ("%0.2f" % total_miles)

        template_values = {"output": output_distance,
                           "distancetype": distancetype}

        path = os.path.join(os.path.dirname(__file__), 'templates/output.html')
        self.response.out.write(template.render(path, template_values))

app = webapp.WSGIApplication([('.*', MainHandler)], debug=False)
