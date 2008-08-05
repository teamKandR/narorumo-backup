import math
import copy
import random

def buildboard(size):
    out = []
    for i in range(0, size):
        out += [[]]
        for j in range(0, size):
            out[i] += [[]]
    for i in range(0, size):
        for j in range(0, size):
            out[i][j] = range(1, size + 1)
    return out

# returns a board in which we're making an assumption that...
# Posssible is a list [row, col, val]
def assume(board, possible):
    [row, col, val] = possible
    out = copyboard(board)
    out[row][col] = val
    return out

def remove_possible(board, possible):
    out = copyboard(board)
    [row, col, val] = possible

    cell = out[row][col]
    # print "number options left here: ", len(cell)
    del cell[cell.index(val)]
    return out

def first_possible(board):
    for i in range(0, len(board)):
        for j in range(0, len(board)):
            if(isinstance(board[i][j], list)):
                cell = board[i][j]
                if([] == cell):
                    return False
                else:
                    return [i,j, cell[0] ] # random.choice(cell) ]
    return False

def copyrow(row):
    out = copy.deepcopy(row)
    return out

def copyboard(board):
    out = copy.deepcopy(board)
    return out

def find_singles(board):
    out = []
    for i in range(0, len(board)):
        for j in range(0, len(board)):
           if(not isinstance(board[i][j], list)):
               out += [[i, j, board[i][j]]]
               # print "foo!", board[i][j], i, j
    return out

def mark_rows(board, singles):
    for single in singles:
        for cell in row_list(board, single[0]):
            if (isinstance(cell, list)):
                if (single[2] in cell):
                    # print "only one thing in the row can have that value!!"
                    del cell[ cell.index(single[2])]
            
def mark_cols(board, singles):
    for single in singles:
        # for cell in [ board[j][single[1]] for j in range(0, len(board))]:
        for cell in col_list(board, single[1]):
            if (isinstance(cell, list)):
                if (single[2] in cell):
                    # print "only one thing in the col can have that value!!"
                    del cell[ cell.index(single[2])]

# region shall be defined as a list of the form [m,n] where
# m,n elt [0,1,2]
def get_region(board, single):
    return region_list(board, region_of([single[0], single[1]]))
    
def region_of(coords):
    divbythree = lambda x: (x / 3)
    return map(divbythree, coords)

def row_list(board, row):
    return board[row]

def col_list(board, col):
    out = []
    for i in range(0, 9):
        out += [ board[i][col] ]
    return out

def region_list(board, reg):
    m = reg[0]
    n = reg[1]

    out = []
    for i in range(0,3):
        for j in range(0,3):
            out += [ board[(m * 3) + i][(n * 3) + j] ]
    return out

def mark_regions(board, singles):
    for single in singles:
        for cell in get_region(board, single):
            if (isinstance(cell, list)):
                if (single[2] in cell):
                    # print "only one thing in the region can have that value!!"
                    del cell[ cell.index(single[2])]

def atoms(things):
    out = []
    for thing in things:
        if(not isinstance(thing, list)):
            out += [ thing ]
    return out

def uniques(things):
    hash = {}
    for thing in things:
        hash[thing] = 0
    return hash.keys()

# return true if things contains non-unique non-list elements
def has_dupes(things):
    notlists = atoms(things)
    uniq = uniques(notlists)
    return ( len(uniq) != len(notlists) )

# true from this function shall indicate a contradiction.
def only_possible(board):
    for i in range(len(board)):
        for j in range(len(board)):
            cell = board[i][j]
            if( isinstance(cell, list) ):
                if(len(cell) == 1):
                    # print "[only_possible] ", [i,j], " must be ", cell[0]
                    board[i][j] = cell[0]

                    row = copyrow(row_list(board,i))
                    col = copyrow(col_list(board, j))
                    region = copyrow( region_list(board, region_of([i, j])))

                    if( has_dupes(row)
                        or has_dupes(col)
                        or has_dupes(region) ):
                        return True

                    row.remove(cell[0])
                    col.remove(cell[0])
                    region.remove(cell[0])

                    if( (board[i][j] in row)
                        or (board[i][j] in col)
                        or (board[i][j] in region) ):
                        return True

    return False

numbers = range(0,10)
def correct_rows(board):
    for i in range(0, 10):
        row = copyrow(board[i])
        row.sort()
        if(row != numbers):
            return False
    return True

def correct_cols(board):
    return True
def correct_regions(board):
    return True

def correct(board):
    if( correct_rows(board) and correct_cols(board)
        and correct_regions(board)):
        return True
    else:
        return False

def finished(board):
    for i in range(len(board)):
        for j in range(len(board)):
            cell = board[i][j]
            if( isinstance(cell, list) ):
                return False
    return True
## XXX: should read argv[0]

def print_cell(cell):
    if(isinstance(cell, list)):
        print "x",
    else:
        print cell,

def print_row(row):
    map(print_cell, row)
    print ""
    
def print_board(board):
    if(not board):
        print "Failure."
    else:
        map(print_row, board)
#     rowprinter =
#         lambda(row):
#             for cell in row:
#                 if

def solve(board, level):
    wrong = only_possible(board)
    if(wrong): return False

    mark_rows(board, find_singles(board))
    wrong = only_possible(board)
    if(wrong): return False

    mark_cols(board, find_singles(board))
    wrong = only_possible(board)
    if(wrong): return False

    mark_regions(board, find_singles(board))
    wrong = only_possible(board)
    if(wrong): return False

    if(finished(board)):
        # print "It is finished at LEVEL: ", level
        return board

    while(True):
        next_possible = first_possible(board)
        # print "next_possible: ", next_possible

        if(not next_possible):
            return False

        assumption_made = assume(board, next_possible)

        soln = solve(assumption_made, level + 1)

        if(soln):
            # print "Jeebus! Returning success at LEVEL ", level
            return soln
        else:
            # print "Removing a possibility and going on..."
            board = remove_possible(board, next_possible)
            wrong = only_possible(board)
            if(wrong): return False

    print "Failing out at LEVEL: ", level
    return False

def read_board(fn):
    try:
        boardfile = open(fn, 'r')
        board = board_from_stream(boardfile)
    except(IOError):
        print "File not found!"
        board = buildboard(9)
    return board


def col_to_cell(col):
    num = int(col) 
    if(0 == num):
        return range(1,10)
    else:
        return [ num ]

def line_to_row(line):
    cols = line.split()
    return map(col_to_cell, cols)

def board_from_stream(bf):
    lines = bf.readlines()
    return map(line_to_row, lines)
