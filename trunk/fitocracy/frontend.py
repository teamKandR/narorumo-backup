#!/usr/bin/env python

from google.appengine.ext import webapp
from google.appengine.ext.webapp import util
from google.appengine.ext.webapp import template

import os
import base64

import fitocracy

class MainHandler(webapp.RequestHandler):
    def get(self):
        path = os.path.join(os.path.dirname(__file__), 'static/index.html')
        template_values = {}
        self.response.out.write(template.render(path, template_values))

    def post(self):
        text = self.request.get("text")
        if (text.startswith("data:text/plain;base64,") or
            text.startswith("base64,")):
            comma = text.index(",")
            text = base64.b64decode(text[comma+1:])
        try:
            total_miles = fitocracy.total_miles_from_text(text)
        except:
            total_miles = 0

        if self.request.get("kilometers"):
            distancetype = "kilometers"
            total_kilometers = 1.609344 * total_miles
            output_distance = ("%0.2f" % total_kilometers)
        else:
            distancetype = "miles"
            output_distance = ("%0.2f" % total_miles)

        if total_miles == 0:
            output_distance = ("None at all. Are you sure you copy-pasted " +
                               "the right data into the form?")

        template_values = {"output": output_distance,
                           "distancetype": distancetype}

        path = os.path.join(os.path.dirname(__file__), 'templates/output.html')
        self.response.out.write(template.render(path, template_values))

app = webapp.WSGIApplication([('.*', MainHandler)], debug=False)
