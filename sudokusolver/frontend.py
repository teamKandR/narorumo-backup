import wsgiref.handlers
import os
import sudoku
import Board

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
    board = Board.Board()

    Board.set_constraints(board, constraints)

    solved = board.solve()
    if (solved):
      template_values = {'board': solved.as_list()}
    else:
      template_values = {}

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
