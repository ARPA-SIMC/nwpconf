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
## @brief Module with functions for retrieving observations and initial/boundary conditions from arkimet archive.
## @details This module provides functions for retrieving observations
## in BUFR format and initial/boundary conditions, tipically in GRIB
## format, from the desired dataset of an [Arkimet
## archive](https://github.com/ARPA-SIMC/arkimet).
## 
## It is an optional module and it has to be sourced after the
## _nwptime.sh_ module.

## @fn getarki_obsbufr()
## @brief Retrieve observations in BUFR format from arkimet archive.
## @details This function retrieves observations in BUFR format from
## the arkimet dataset(s) specified in the configuration variable
## `$BUFR_ARKI_DS`, for the data assimilation interval specified in
## the configuration. It should be called after having loaded the
## module nwptime.sh for setting up the time-related environment
## variables. The interval of data retrieved is extended by a
## configurable amount of hours (default 3), before and after the
## strict assimilation interval.
## @param $1 name of the output bufr file
## @param $2 optional value of extra time interval in hours before and after assimilation, default 3
getarki_obsbufr() {

    local d t ds de dt
    dt=${2:-3}
    d=`date_sub $DATES $TIMES $dt`
    t=`time_sub $DATES $TIMES $dt`
    ds=`getarki_datetime $d $t`
    d=`date_add $DATEE $TIMEE $dt`
    t=`time_add $DATEE $TIMEE $dt`
    de=`getarki_datetime $d $t`

    [ -n "$WAITFUNCTION" ] && $WAITFUNCTION

    arki-query --data -o $1 \
	"origin: BUFR; reftime:>=$ds,<=$de;" $BUFR_ARKI_DS

}


## @fn getarki_icbc()
## @brief Retrieve gridded initial and/or boundary conditions from arkimet archive.
## @details This function retrieves gridded fields, tipically in GRIB
## format, to be used as initial and/or boundary conditions, possibly
## through an interpolation process, from the arkimet dataset(s)
## specified in the configuration variable `$PARENTMODEL_ARKI_DS`, for the
## model run interval specified in the configuration. A specific model
## system module must have been loaded in order to provide the
## function inputmodel_name() for renaming the files. It should be
## called after having loaded the module nwptime.sh for setting up the
## time-related environment variables and within a time loop on input
## models, such as:
## 
##     nwpbctimeloop_init
##     while nwpbctimeloop_loop; do
##         getarki_icbc
##     done
## 
## Additional query keys for the arki-query can be specified by means
## of the variable `$MODEL_ARKI_PARAM`, terminated by `;`,
## e.g. `MODEL_ARKI_PARAM="proddef:GRIB:nn=$ENS_MEMB;"` for selecting
## a specific ensemble member as input.


getarki_icbc() {
    local h hinput timerange ana d2h t2h

    for h in `seq $MODEL_START_SLICE $MODEL_FREQ_SLICE $MODEL_STOP_SLICE`; do
	[ -n "$WAITFUNCTION" ] && $WAITFUNCTION $h

#	if [ "$MODEL_BCANA" = "Y" ]; then # no more necessary
	hinput=$(($h+$MODEL_DELTABD_SLICE))
	timerange="timerange:Timedef,${hinput}h,254"

	reftime=`getarki_datetime $DATES_SLICE $TIMES_SLICE`

	ntry=2
	ofile=`inputmodel_name $h`
	while [ "$ntry" -gt 0 ]; do
	    arki-query --data -o $ofile \
		"reftime:=$reftime;$timerange;$MODEL_ARKI_PARAM" $PARENTMODEL_ARKI_DS
# if file is empty retry, otherwise exit
	    if [ -s "$ofile" ]; then
		break
	    fi
	    echo "retrying arki-query"
	    sleep 10
	    ntry=$(($ntry - 1))
	done
	
	if [ "$h" -eq "0" ]; then
	    ana=`inputmodel_name a`
	    [ -f "$ana" -o -h "$ana" ] || ln -s `inputmodel_name $h` $ana
	fi
# if defined, increment progress meter
	type meter_increment 2>/dev/null && meter_increment || true
    done

}

# The date and time as requested by reftime arki-query key
getarki_datetime() {
    if [ "${#1}" -gt 8 ]; then
	$DATECOM --date "${1:0:8} ${1:8:4}" '+%Y-%m-%d %H:00'
    else
	$DATECOM --date "$1 $2:00" '+%Y-%m-%d %H:00'
    fi
}

# start exporting all assignments
set -a
# checks
check_dep getarki nwptime
# default time to wait between checs for availability of input data
GETARKI_WAITSTART=30
# stop exporting all assignments
set +a
