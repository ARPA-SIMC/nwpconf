#!/usr/bin/python
# Copyright (C) 2015 Davide Cesari
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# this program reads a MARS requests and either converts it to a
# dataset (dictionary) request if it is a dataset type (~public
# dataset) request or it passes it unchanged it to a MARS server if a
# MARS service is requested (the last is probably not necessary since
# it seems that it is possible to provide a dictionary request also
# for MARS services as steted in:
# https://software.ecmwf.int/wiki/display/WEBAPI/WebAPI+FAQ

import sys
import re
import os
import shutil
import ecmwfapi

def addkeyword(rematch, keydict):
    try:
        key = rematch.group(1)
        for val in rematch.group(3,4,5): # search among candidates
            if val:
                keydict[key] = val
                return
    except IndexError:
        sys.stderr.write('exception in addkeyword\n')
        pass


if len(sys.argv) > 1:
    req = file(sys.argv[1]).read()
else:
    req = sys.stdin.read()


def mars_retrieve(req, target):
    if "WEBMARS_TARGET" in os.environ:
        target = os.environ["WEBMARS_TARGET"]
    service = ecmwfapi.ECMWFService("mars")
    service.execute(req, target)

def dict_retrieve(req, append):
    if append: # save real target name and d/l request to a tmp target file
        savetarget = req['target']
        req['target'] = req['target']+'.tmp'
    server = ecmwfapi.ECMWFDataServer()
    server.retrieve(req)
    if append: # append to real target by file objects
        if os.path.isfile(req['target']):
            targetobj = open(savetarget, 'ab')
            shutil.copyfileobj(open(req['target'],'rb'), targetobj)
            targetobj.close()
            os.unlink(req['target']) # rm tmp target file
        req['target'] = savetarget # restore real target


marskey = re.compile('^([a-zA-Z]+)\s*=\s*(([^\'",]*)|"([^"]*)"|\'([^\']*)\')\s*,*$')
marsretr = re.compile('\s*retrieve\s*,\s*', re.I)
marscont = re.compile('.*,\s*')
marsstrp = re.compile('^\s*(.*\s*,*)\s*$')
marsignore = re.compile('^\s*(#.*)*$')

reql = req.split('\n')
reqdict = {}
state = 0 # 0 out, 1 in retrieve, 2 after retrieve
mustappend = False

for line in reql:
    if marsignore.search(line): continue # skip empty & comments
    contline = marscont.search(line) # remember whether it is a continuation line
    strip = marsstrp.search(line) # strip spaces and comma
    if strip:
        try:
            line = strip.group(1)
        except IndexError:
            pass
#    else:
    if state == 0 or state == 2:
        m = marsretr.search(line)
        if m:
            state = 1
            continue
        else:
            sys.stderr.write('error, MARS keyword not preceded by "RETRIEVE":\n')
            sys.stderr.write(line+'\n')
            sys.exit(1)
    elif state == 1:
        marskeymatch = marskey.search(line)
        if not marskeymatch:
            sys.stderr.write('error, MARS keyword not recognized:\n')
            sys.stderr.write(line+'\n')
            sys.exit(1)
        addkeyword(marskeymatch, reqdict)
        if not contline: # no continuation, end of query
            state = 2

    if state == 2:
        if 'dataset' in reqdict:
            dict_retrieve(reqdict, mustappend)
            mustappend = True
        else:
            mars_retrieve(req, reqdict.get('target'))
            sys.exit(0)
