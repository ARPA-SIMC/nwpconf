#!/bin/bash
# sample script for testing the basic features of nwpconf package

# setup common to user scripts
# basic variables
export NWPCONFDIR=$PWD/conf
export NWPCONFBINDIR=$PWD/../bin
export NWPCONF=production/dailymodelrun/forecast
# source the main library module
set -e
. $NWPCONFBINDIR/nwpconf.sh
# end of setup

# generate file from template
conf_template modelrun.conf
