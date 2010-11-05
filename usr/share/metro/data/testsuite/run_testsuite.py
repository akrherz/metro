#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-
#
# METRo : Model of the Environment and Temperature of Roads
# METRo is Free and is proudly provided by the Government of Canada
# Copyright (C) Her Majesty The Queen in Right of Canada, Environment Canada, 2006 

#  Questions or bugs report: metro@ec.gc.ca
#  METRo repository: https://gna.org/projects/metro/
#  Documentation: http://documentation.wikia.com/wiki/METRo
#
#
# Code contributed by:
#  Francois Fortin - Canadian meteorological center
#
#  $LastChangedDate$
#  $LastChangedRevision$
#
########################################################################
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#
#

import os
import re
import subprocess
import math

metroExec = "../../../../../src/frontend/metro.py"
testsuiteDir = "."


def floatEqual(float1, float2):
    threshold = 0.5
    if ( math.fabs(float1 - float2) > threshold ):
        return False
    else:
        return True



def analyseDiffOutput(diffOutput):

    diffOutputLines = diffOutput.split('\n')

    productionDateMatcher = re.compile('^.*<production-date>.*')
    tagMatcher =  re.compile('^.*<.*>(.*)<.*>.*<.*>(.*)<.*>.*')

    rslt = True
    
    for line in diffOutputLines:
        

        if productionDateMatcher.match(line):
            pass
            # print "SKIP line=" + line
        else:
            if line != "":
             #   print "investigate further"
             #   print "line='" + line + "'"
                pm = tagMatcher.match(line)
                if floatEqual( float(pm.group(1)), float(pm.group(2)) ) == False:
                    print "        Fail diff on: " + line
                    # print pm.group(1)
                    # print pm.group(2)
                    rslt = False

    return rslt
        

def execCommand(command):
    cmdList = command.split()
    proc = subprocess.Popen(cmdList, 
                        stdout=subprocess.PIPE,
                        )
    stdout_value = proc.communicate()[0]

    rslt = {}

    rslt["STDOUT"] = stdout_value
    rslt["RETURN_CODE"] = proc.poll()
    
    return rslt



dirList = os.listdir(testsuiteDir)

caseList = []

caseMatcher=re.compile('^case_[0-9][0-9][0-9]$')
#only keep case
for item in dirList:
    if caseMatcher.match(item):
        caseList.append(item)

caseList.sort()

nbSucces = 0
nbFailures = 0
nbCases = len(caseList)

failCases = []

for case in caseList:
    print ""
    print ""
    print ""
    print "Test Case: " + case + ""
    print "START"

    roadcastResultPath = "roadcast_" + case
    runCommand = "python " + metroExec + " --silent --verbose-level 0 --input-forecast " + case + "/forecast.xml --input-observation " \
                  + case + "/observation.xml --input-station " + case + "/station.xml --output-roadcast " + roadcastResultPath

    print "    command: " + runCommand

    rslt = execCommand(runCommand)
    if rslt["RETURN_CODE"] == 0:
        print "  RUN SUCCES!"
        
        diffCommand = "diff --side-by-side --suppress-common-lines " + roadcastResultPath + " " + case + "/roadcast.xml"
        rslt = execCommand(diffCommand)

        if analyseDiffOutput(rslt["STDOUT"]) == True:
            print "  DIFF SUCCES!"
            nbSucces += 1
        else:
            print "  ***DIFF FAIL!"
            nbFailures += 1
            failCases.append(case)
            
        
    else:
        print "  ***RUN FAIL!"
        nbFailures += 1
        failCases.append(case)
    
    print "END " + case
    
print ""
print ""
print ""
print "================================================================================"
print " number of cases   : " + str(nbCases)
print " number of succes  : " + str(nbSucces)
print " number of failures: " + str(nbFailures)
print ""
if nbFailures > 0:
    print "failures: " + str(failCases)
print "================================================================================"
