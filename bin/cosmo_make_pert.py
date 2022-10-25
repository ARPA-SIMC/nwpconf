#!/usr/bin/python3

import sys
import csv

# read a csv file with a table of namelist parameters to be perturbed
# and their values and write a piece of namelist for each line/member
# first argument is the csv input file
# second argument is the namelist output file, it must contain %d to
# be replaced by the member number

out = sys.argv[2]
count = 0
with open(sys.argv[1]) as csvfile:
    reader = csv.DictReader(csvfile, delimiter=",")

    for row in reader:
        count+=1
        ofp = open(out % count, "w")
        for naml in row:
            if naml != "":
                if "/" in naml:
                    for naml2 in naml.split("/"):
                        ofp.write(f"{naml2} = {row[naml]}\n")
                else:
                    ofp.write(f"{naml} = {row[naml]}\n")
        ofp.close()

