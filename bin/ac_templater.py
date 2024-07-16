#!/usr/bin/python2

import sys
import os
import re

def open_include(name):
    res = None
    confdirlist = os.getenv('confdirlist')
    if os.path.basename(name) != name or confdirlist is None:
        res = name
    else:
# emulate conf_getfile
        ens_memb = os.getenv('ENS_MEMB')
        if ens_memb is not None: ens_memb = '.'+ens_memb
        for searchdir in confdirlist.split():
            tryfile = os.path.join(searchdir, name)
            if os.path.exists(tryfile): res = tryfile
            if ens_memb is not None:
                tryfile2 = tryfile+'.ens'
                if os.path.exists(tryfile2): res = tryfile2
                tryfile2 = tryfile+ens_memb
                if os.path.exists(tryfile2): res = tryfile2
    if res is not None: return open(res)
    sys.stderr.write("include file "+name+" was not found")
    return None


def ac_templater(fd_in, fd_out):
    """
    Replace values represented as @VAR@
    """
    RE_INCLUDE = re.compile(r"^ *@INCLUDE ([^@]+)@ *$")
    RE_LINE = re.compile(r"@([^@]+)@")

    
    def subst(mo):
        try:
            return os.environ[mo.group(1)]
        except KeyError:
            missingkeys.append(mo.group(1))
            return "None"

    for line in fd_in:
        mo = RE_INCLUDE.match(line)
        if mo is not None:
            fd_inc = open_include(mo.group(1))
            ac_templater(fd_inc, fd_out)
            fd_inc.close()
        else:
            fd_out.write(RE_LINE.sub(subst, line))


missingkeys = []
if len(sys.argv) > 1:
    fd_in = open(sys.argv[1])
else:
    fd_in = sys.stdin
ac_templater(fd_in, sys.stdout)

if len(missingkeys) > 0:
    sys.stderr.write("the following required environment variables were not set:\n")
    sys.stderr.write(repr(missingkeys)+"\n")
    sys.exit(1)
