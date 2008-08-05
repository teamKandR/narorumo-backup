import sys
sys.path.append('/home/alex/public_html/sudoku/')

import sudoku
reload(sudoku)

def row_to_tr(row):
    out = "<tr>\n"
    for cell in row:
        out += ( "\t<td>" + str(cell) + "</td>\n")
    out += "</tr>\n"
    return out

def board_to_table(board):
    out = "<table border=1>\n"
	
    if(not board):
        out += "<tr><td>(Failure.)</td></tr>"
    else:
        for row in board:
            out += row_to_tr(row)

    out += "</table>"
    return out

def build_input_table():
    out = "<table border=1>\n"
    for i in range(0,9):
        out += "<tr>"
        for j in range(0,9):
            out += ("\t\t<td><input type='text' size='1' name='cell"
                + str(i) + "_" + str(j) + "'/></td>\n" )
        out += "</tr>\n"
    out += "</table>"
    return out

def form_to_board(form):
    out = sudoku.buildboard(9)
    for i in range(0,9):
        for j in range(0,9):
            key = ( 'cell' + str(i) + '_' + str(j) )
            if(form.has_key(key)):
                try:
                    val = int( form[key] )
                    if( 1 <= val <= 9 ):
                        out[i][j] = [ val ]
                    else:
                        return False
                except(ValueError):
                    out[i][j] = range(1,10)
    return out
