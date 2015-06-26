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
## @brief Modules with functions for extracting observations and initial/boundary conditions from arkimet archive.
## @details This module provides functions for extracting observations
## in BUFR format and initial/boundary conditions, tipically in GRIB
## format, from the desired dataset of an [Arkimet
## archive](http://arkimet.sourceforge.net/).
## 
## It is an optional module and it has to be sourced after the
## _nwptime.sh_ module.

## @fn getarki_obsbufr()
## @brief Retrieve observations in BUFR format from arkimet archive.
## @details This function retrieves observations in BUFR format from
## the arkimet dataset(s) specified in the configuration variable
## `$ARKI_BUFR_DS`, for the data assimilation interval specified in
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
    d=`date_sub $D1 $T1 $dt`
    t=`time_sub $D1 $T1 $dt`
    ds=`datetime_arki $d $t`
    d=`date_add $DATE $TIME $dt`
    t=`time_add $DATE $TIME $dt`
    de=`datetime_arki $d $t`

    [ -n "$WAITFUNCTION" ] && $WAITFUNCTION

    arki-query --data -o $1 \
	"origin: BUFR; reftime:>=$ds,<=$de;" $ARKI_BUFR_DS

}


## @fn getarki_icbc()
## @brief Retrieve gridded initial and/or boundary conditions from arkimet archive.

## @details This function retrieves gridded fields to be used as
## initial and/or boundary conditions, tipically in GRIB format, from
## the arkimet dataset(s) specified in the configuration variable
## `$ARKI_ICBC_DS`, for the model run interval specified in the
## configuration. A specific model system module must have been loaded
## in order to provide the function inputmodel_name() for renaming the
## files. It should be called after having loaded the module
## nwptime.sh for setting up the time-related environment variables
## and within a time loop on input models, such as:
## 
##     nwpbctimeloop_init
##     while nwpbctimeloop_loop; do
##         getarki_icbc
##     done
getarki_icbc() {
    local h hinput timerange ana

    for h in `seq $MODEL_START $MODEL_FREQ_INPUT $MODEL_STOP`; do
	[ -n "$WAITFUNCTION" ] && $WAITFUNCTION $h

	if [ "$MODEL_BCANA" = "Y" ]; then
	    d2h=`date_add $D2 $T2 $h`
	    t2h=`time_add $D2 $T2 $h`
	    reftime=`datetime_arki $d2h $t2h`
	    timerange="timerange:Timedef,0h,254"
	else
	    reftime=`datetime_arki $D2 $T2`
	    hinput=$(($h+$DELTABD))
	    timerange="timerange:Timedef,${hinput}h,254"
	fi
	arki-query --data -o `inputmodel_name $h` \
	    "reftime:=$reftime;$timerange" $ARKI_ICBC_DS
#	[ "$h" -eq "0" -a ] or [ "$h" -eq "$MODEL_START" ] ?
	if [ "$h" -eq "0" ]; then
	    ana=`inputmodel_name a`
	    [ -f "$ana" -o -h "$ana" ] || ln -s `inputmodel_name $h` $ana
	fi
	sms_meter_increment
    done

}

# The date and time as requested by reftime arki-query key
datetime_arki() {
    $DATECOM --date "$1 $2:00" '+%Y-%m-%d %H:00'
}

# start exporting all assignments
set -a
# checks
check_dep getarki nwptime
#check_defined DATE TIME D1 T1 D2 T2 MODEL_START MODEL_FREQ_INPUT MODEL_STOP MODEL_BCANA
# stop exporting all assignments
set +a
