#!/usr/bin/env python3

"""
Use this script to find out how many miles you've logged in total on Fitocracy! 

Go to your profile > History > Running, then click CSV. It will produce, in
your browser, a bunch of CSV data. Save that to a file, then run this script on
that file!

Should work both under recent Python 2 and Python 3.
"""

from __future__ import print_function
from __future__ import division
import csv
import sys

multiplier_table = { "mi": 1.0,
                     "km": 0.621371192,
                     "ft": (1 / 5280),
                     "m": 0.000621371192,
                     "yd": (1 / 1760) }

def to_miles(dist, units):
    asfloat = float(dist)
    multiplier = multiplier_table[units]
    return asfloat * multiplier

def main():
    if len(sys.argv) != 2:
        print("usage: python3 {0} myrunningstats.csv".format(sys.argv[0]))
        return
    csvReader = csv.reader(open(sys.argv[1]))

    total_miles = 0
    headers = next(csvReader)
    distIndex = headers.index("Distance")
    distUnitIndex = distIndex + 1
    dateIndex = headers.index("Date (YYYYMMDD)")

    for row in csvReader:
        dist = row[distIndex]
        distunit = row[distUnitIndex]
        date = row[dateIndex]
        if(dist):
            miles = to_miles(dist, distunit)
            print("on", date, "ran this many miles:", miles)
            total_miles += miles
    print("You have run this many miles on Fitocracy:", total_miles)

if __name__ == "__main__": main()
