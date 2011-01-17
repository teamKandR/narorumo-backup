#!/usr/bin/env python

"""
The zebra puzzle, using Gustavo Niemeyer's constraint.py
Puzzle from here:
http://www.theweinerworks.com/?p=315

constraint.py is from here:
http://labix.org/python-constraint
"""

import constraint

guys = "english spanish japanese italian norwegian".split()
variables = "job posn color pet drink".split()

def buildProblem():
    problem = constraint.Problem()
    values = {
        "job" : "painter diplomat physician violinist photographer".split(),
        "posn" : map(int, "1 2 3 4 5".split()),
        "color" : "red green white blue yellow".split(),
        "pet" : "dog snail zebra fox horse".split(),
        "drink" : "tea oj mineralwater milk coffee".split(),
        }

    for guy in guys:
        for variable in variables:
            problem.addVariable(guy + variable, values[variable])
    return problem

def constrain(problem):
    ### every job, posn, color and pet are different
    for varname in variables:
        for guy1 in guys:
            for guy2 in guys:
                if guy1 == guy2: continue
                v1 = guy1 + varname
                v2 = guy2 + varname 
                problem.addConstraint(
                    lambda g1var, g2var: g1var != g2var, [v1, v2])

    ## english lives in the red house.
    problem.addConstraint( lambda v: v == "red", ["englishcolor"])

    ## spanish owns a dog.
    problem.addConstraint( lambda v: v == "dog", ["spanishpet"])

    ## japanese is a painter
    problem.addConstraint( lambda v: v == "painter", ["japanesejob"])

    ## italian drinks tea
    problem.addConstraint( lambda v: v == "tea", ["italiandrink"])

    ## norwegian lives in house 1
    problem.addConstraint( lambda v: v == 1, ["norwegianposn"])

    ## green house is immediately to the right of the white one
    def greenhouse_rightof_whitehouse(houseposn1, houseposn2,
                                      housecolor1, housecolor2):
        if housecolor1 == "green" and housecolor2 == "white":
            return houseposn1 == (houseposn2 + 1)

        if housecolor1 == "white" and housecolor2 == "green":
            return houseposn1 == (houseposn2 - 1)
        return True

    for guy1 in guys:
        for guy2 in guys:
            if guy1 == guy2: continue
            houseposn1 = guy1  + "posn"
            houseposn2 = guy2  + "posn"
            housecolor1 = guy1 + "color"
            housecolor2 = guy2 + "color"

            problem.addConstraint(greenhouse_rightof_whitehouse,
                [houseposn1, houseposn2, housecolor1, housecolor2])


    ## the photographer breeds snails.
    def photographer_breeds_snails(job, pet):
        if job == "photographer":
            return pet == "snail"
        if pet == "snail":
            return job == "photographer"
        return True

    for guy in guys:
        job = guy + "job"
        pet = guy + "pet"
        problem.addConstraint(photographer_breeds_snails, [job, pet])

    ## diplomat lives in the yellow house
    def diplomat_in_yellow(job, color):
        if job == "diplomat":
            return color == "yellow"
        if color == "yellow":
            return job == "diplomat"
        return True

    for guy in guys:
        job = guy + "job"
        color = guy + "color"
        problem.addConstraint(diplomat_in_yellow, [job, color])

    ## milk drunk in the middle house
    def milk_in_middle(drink, posn):
        if drink == "milk":
            return posn == 3
        if posn == 3:
            return drink == "milk"
        return True

    for guy in guys:
        drink = guy + "drink"
        posn = guy + "posn"
        problem.addConstraint(milk_in_middle, [drink, posn])

    ## green house guy drinks coffee
    def green_drinks_coffee(color, drink):
        if drink == "coffee":
            return color == "green" 
        if color == "green":
            return drink == "coffee"
        return True

    for guy in guys:
        drink = guy + "drink"
        color = guy + "color"
        problem.addConstraint(green_drinks_coffee, [color, drink])

    ## norwegian guy next to blue house
    ## so for any guy2, there's guy2posn and guy2color,
    ## and if guy2color == "blue" then norwegianposn == guy2posn +- 1
    def norwegian_next_to_blue(posn, color, norwegianposn):
        if color == "blue":
            if posn == norwegianposn + 1:
                return True
            if posn == norwegianposn - 1:
                return True
            return False
        return True

    for guy2 in guys:
        guy2posn = guy2 + "posn"
        guy2color = guy2 + "color"
        problem.addConstraint(norwegian_next_to_blue,
            [guy2posn, guy2color, "norwegianposn"])

    # the violinist drinks orange juice
    def violinist_drinks_oj(job, drink):
        if job == "violinist":
            return drink == "oj"
        if drink == "oj":
            return job == "violinist"
        return True

    for guy in guys:
        job = guy + "job"
        drink = guy + "drink"
        problem.addConstraint(violinist_drinks_oj, [job, drink])

    # fox is in a house next to that of the physician
    def fox_next_to_physician(pet1, posn1, job2, posn2):
        if pet1 == "fox" and job2 == "physician":
            if posn1 == posn2 + 1:
                return True
            if posn1 == posn2 - 1:
                return True
            return False
        return True

    for guy1 in guys:
        for guy2 in guys:
            guy1pet = guy1 + "pet"
            guy1posn = guy1 + "posn"
            guy2job = guy2 + "job"
            guy2posn = guy2 + "posn"
            problem.addConstraint(fox_next_to_physician,
                [guy1pet, guy1posn, guy2job, guy2posn])

    # horse in a house next to that of the diplomat
    def horse_next_to_diplomat(pet1, posn1, job2, posn2):
        if pet1 == "horse" and job2 == "diplomat":
            if posn1 == posn2 + 1:
                return True
            if posn1 == posn2 - 1:
                return True
            return False
        return True

    for guy1 in guys:
        for guy2 in guys:
            guy1pet = guy1 + "pet"
            guy1posn = guy1 + "posn"
            guy2job = guy2 + "job"
            guy2posn = guy2 + "posn"
            problem.addConstraint(horse_next_to_diplomat,
                [guy1pet, guy1posn, guy2job, guy2posn])

def printsolution(soln):
    for guy in guys:
        print(guy)
        for var in variables:
            print("\t%s:%s" % (var, soln[guy+var]))

def main():
    problem = buildProblem()
    constrain(problem)

    solns = problem.getSolutions()
    for i, soln in enumerate(solns):
        print "*** solution", i, "***"
        printsolution(soln)

if __name__ == "__main__": main()
