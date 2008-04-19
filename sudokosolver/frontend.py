import wsgiref.handlers
import os
import sudoku

from google.appengine.ext.webapp import template
from google.appengine.api import users
from google.appengine.ext import webapp

class MainPage(webapp.RequestHandler):
  def get(self):
    template_values = {
        'rows': range(9),
        'cols': range(9)
    }

    path = os.path.join(os.path.dirname(__file__), 'input.html')
    self.response.out.write(template.render(path, template_values))

  def post(self):
    constraints = inputvalues(self.request)
    board = sudoku.build_board()

    sudoku.set_constraints(board, constraints)
    solved = sudoku.solve(board)
    template_values = {'board':solved}

    path = os.path.join(os.path.dirname(__file__), 'showanswer.html')
    self.response.out.write(template.render(path, template_values))

def inputvalues(req):
  out = []
  for row in range(9):
    for col in range(9):
      key = ("cell%d_%d" % (row, col))

      val = req.get(key)
      if (val != ''):
        try:
          val = int(val)
          out.append( (row,col,val) )
        except ValueError, e:
          pass
  return out

def main():
  application = webapp.WSGIApplication( [('/', MainPage)], debug=True)
  wsgiref.handlers.CGIHandler().run(application)

if __name__ == "__main__":
  main()
