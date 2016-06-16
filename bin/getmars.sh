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

## @file
## @brief Module with functions for retrieving observations and initial/boundary conditions from MARS archive.
## @details This module provides functions for retrieving observations
## in BUFR format and initial/boundary conditions, tipically in GRIB
## format, from the desired dataset of a [MARS
## archive](http://www.ecmwf.int/).
## 
## It is an optional module and it has to be sourced after the
## _nwptime.sh_ module.

_getmars_generic_icbc_(){

    local ttype
    if [ "$3" = 0 ]; then # euristic and hardcoded, check better
	ttype="type = AN,"
    else
	ttype="type = FC,"
    fi

    echo "retrieve,
$MODEL_MARS_BASE
$ttype
date = $1,
time = $2,
step = $3,
target = \"$5\",
$MODEL_MARS_PARAM"


# constant fields
    if [ "$4" = "Y" -a -n "$MODEL_MARS_CONST" ]; then
	echo "retrieve,
$MODEL_MARS_CONST"
    fi

}

## @fn getarki_icbc()
## @brief Retrieve gridded initial and/or boundary conditions from MARS archive.
## @details This function retrieves gridded fields, tipically in GRIB
## format, to be used as initial and/or boundary conditions, possibly
## through an interpolation process, from the ECMWF MARS archive for
## the model run interval specified in the configuration.  The basic
## keywords of the MARS query are specified in the configuration
## variable `$MODEL_MARS_BASE`, while the more specific keywords, such
## as list of variables, levels, timerange and reference time are
## automatically added to the basic query by the function.  A specific
## model system module must have been loaded in order to provide the
## function inputmodel_name() for renaming the files. It should be
## called after having loaded the module nwptime.sh for setting up the
## time-related environment variables and within a time loop on input
## models, such as:
## 
##     nwpbctimeloop_init
##     while nwpbctimeloop_loop; do
##         getmars_icbc
##     done
getmars_icbc() {
    local h hinput timerange ana d2h t2h

    for h in `seq $MODEL_START_SLICE $MODEL_FREQ_SLICE $MODEL_STOP_SLICE`; do
	outfile=`inputmodel_name $h`
	rm -f $outfile

#	if [ "$MODEL_BCANA" = "Y" ]; then # no more necessary
	hinput=$(($h+$MODEL_DELTABD_SLICE))
	_getmars_generic_icbc_ $DATES_SLICE $TIMES_SLICE $hinput `[ "$h" -eq "0" ] && echo Y || echo N` $outfile | $NWPCONFBINDIR/unmarsify.py

	if [ "$h" -eq "0" ]; then
	    ana=`inputmodel_name a`
	    [ -f "$ana" -o -h "$ana" ] || ln -s `inputmodel_name $h` $ana
	fi
# if defined, increment progress meter
	type meter_increment 2>/dev/null && meter_increment || true
    done

}

# start exporting all assignments
set -a
# checks
check_dep getmars nwptime
# stop exporting all assignments
set +a
