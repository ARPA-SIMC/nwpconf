#!/usr/bin/python

import sys, os
import collections
import datetime
import numpy
from gribapi import *
#richiede export PYTHONPATH=/usr/lib64/python2.7/site-packages/grib_api/

nx = int(os.environ['SWAN_NX'])+1
ny = int(os.environ['SWAN_NY'])+1
keys = collections.OrderedDict((
    ('dataDate', 0),
    ('dataTime', 0),
    ('table2Version', 140), # 2?
    ('centre', 200),
    ('generatingProcessIdentifier', 101),
    ('indicatorOfParameter', 0),
    ('P1', 0),
    ('P2', 0),
    ('Ni', nx),
    ('Nj', ny),
    ('longitudeOfFirstGridPointInDegrees', os.environ['SWAN_XMIN']),
    ('latitudeOfFirstGridPointInDegrees', os.environ['SWAN_YMIN']),
    ('longitudeOfLastGridPointInDegrees', os.environ['SWAN_XMAX']),
    ('latitudeOfLastGridPointInDegrees', os.environ['SWAN_YMAX']),
    # ('iDirectionIncrementInDegrees', (float(os.environ['SWAN_XMAX'])
    #                                   -float(os.environ['SWAN_XMIN']))/
    #  float(os.environ['SWAN_NX'])),
    # ('jDirectionIncrementInDegrees', (float(os.environ['SWAN_YMAX'])
    #                                   -float(os.environ['SWAN_YMIN']))/
    #  float(os.environ['SWAN_NY'])),
    ('iDirectionIncrementInDegrees', None),
    ('jDirectionIncrementInDegrees', None),
    ('ijDirectionIncrementGiven', 0),
    ('scanningMode', 64),
    ('bitsPerValue', 24)

))
values = numpy.empty(nx*ny, float)
vmiss = -999.

if os.environ['MODEL_BACK'] == '0': # forecast mode
    keys['dataDate'] = int(os.environ['DATE'])
    keys['dataTime'] = int(os.environ['TIME'][0:2]+'00')
else: # analysis mode
    reftime = datetime.datetime(
        year=int(os.environ['DATES'][0:4]),
        month=int(os.environ['DATES'][4:6]),
        day=int(os.environ['DATES'][6:8]),
        hour=int(os.environ['TIMES'][0:2]) # 0:2 future extension
    )
    keys['P1'] = 0

sample_id = grib_new_from_samples("regular_ll_sfc_grib1")
# open single output file
fouto = open(sys.argv[-1],'w')
# loop over input files
for filein in sys.argv[1:-1]:
# extract parameter from file name
# /path/to/2016040313_229_ADRI12.dat
    v = os.path.basename(filein).split('_')
    keys['indicatorOfParameter'] = int(v[1])
# open file
    fino = open(filein, 'r')
#    for n in range(7): fino.readline()
# skip comment header
    lineread = fino.readline()
    while lineread.startswith('%'): lineread = fino.readline()
# loop until EOF
    n = 0
    while len(lineread) > 0:
        if os.environ['MODEL_BACK'] == '0': # forecast model
            keys['P1'] = n
        else: # analysis mode
            vertime = reftime + datetime.timedelta(hours=n)
            keys['dataDate'] = int(vertime.strftime('%Y%m%d'))
            keys['dataTime'] = int(vertime.strftime('%H00'))
    
# loop on a single time level
        for ln in range(nx*ny):
            values[ln] = float(lineread)
            if values[ln] == -9.: values[ln] = vmiss
            lineread = fino.readline()

        clone_id = grib_clone(sample_id)
# set custom grib keys
        for key in keys:
            if keys[key] is None: grib_set_missing(clone_id, key)
            else: grib_set(clone_id, key, keys[key])
# set bitmap
        grib_set(clone_id, 'missingValue', vmiss)
        grib_set(clone_id, 'bitmapPresent', 1)
# set values
        grib_set_values(clone_id, values)
# write message
        grib_write(clone_id, fouto)
        grib_release(clone_id)
        n = n + 1 # 1 hour fixed increment

    fino.close()
fouto.close()
grib_release(sample_id)
