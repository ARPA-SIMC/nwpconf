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
## @brief Module with functions for managing the reference time of NWP runs.
## @details This module provides functions for performing various
## types of computations related to the reference and forecast time of
## a NWP model run and of the driving model data. Assimilation and
## forecast modes are supported, as well analysed and forecast
## boundary conditions with a desired reference time shift. The terms
## _input model_ and _driving model_ are used interchangeably
## hereafter.
## 
## The following environmental variables should be set in order to
## describe the model reference timing:
## 
## - `$DATE` (YYYYMMDD) and `$TIME` (hhmm) indicate the current date
##   and time, i.e. the start of the run for a forecast or the end of
##   the run for an assimilation run
## 
## - `$MODEL_BACK` indicates the number of hours to go back from
##   `$DATE$TIME` for starting an assimilation run (0 for a forecast
##   run)
## 
## - `$MODEL_STOP` indicates the duration of the run in hours (`==
##   $MODEL_BACK` for an assimilation run)
## 
## - `$MODEL_BCANA` if set to `Y` indicates that analysis data are
##   used as boundary conditions
## 
## - `$MODEL_DELTABD` indicates the difference in hours between
##   `$DATE$TIME` and the initialisation time of last available input
##   model forecast, it is ignored if `$MODEL_BCANA == Y`
## 
## - `$MODEL_FREQANA_INPUT` indicates the interval in hours between
##   available analysis data from the driving model, used in the case
##   of `$MODEL_BCANA == Y`, it is assumed that analyses are produced
##   at 00 UTC and successively every `$MODEL_FREQANA_INPUT` hours
## 
## - `$MODEL_FREQINI_INPUT` indicates the interval in hours between
##   initialisation of successive driving model forecasts, it is
##   assumed that forecasts are initialised at 00 UTC and successively
##   every `$MODEL_FREQINI_INPUT` hours
## 
## - `$MODEL_FREQFC_INPUT` indicates the interval in hours between
##   successive driving model forecast steps of a single forecast run
## 
## It is an optional module and it has to be sourced after the main
## _nwpconf.sh_ module.

## @fn nwptime_init()
## @brief Initialise the time-related environment for a NWP run.
## @details This function is implicitly called when the module is
## sourced, it computes the variables `$D1`, `$T1`, `$D2`, `$T2`
## useful for managing boundary conditions in assimilation runs.
nwptime_init() {
# start of NWP run
    D1=`date_sub $DATE $TIME $MODEL_BACK`
    T1=`time_sub $DATE $TIME $MODEL_BACK`
    D2=$D1
    T2=$T1
# MODEL_DELTABD is difference (hours) between $DATE$TIME (end of
# assimilation window / start of forecast) and start of last available
# input forecast providing BC (for BCANA=N)
    if [ "$MODEL_BCANA" = "Y" ]; then
	export MODEL_FREQ_SLICE=$MODEL_FREQANA_INPUT
	MODEL_DELTABD=0
    else
	export MODEL_FREQ_SLICE=$MODEL_FREQFC_INPUT
    fi
}


## @fn nwpbctimeloop_init()
## @brief Initialise a loop on the model runs providing input boundary conditions for a NWP run.
## @details This function has to be called for initialising a loop
## over the model runs that provide the boundary conditions to an
## assimilation run. The most common cases are taken into account,
## like using analysed or forecast BC's, possibly with a shift in time
## and with specified frequency of availability.
nwpbctimeloop_init() {
    MODEL_STOP_SLICE=0
    : ${MODEL_BACK:=0} # set to 0 if unset
# DELTABD_SLICE is difference between start of assimilation and start of
# input forecast suitable for providing BC
    if [ "$MODEL_BCANA" != "Y" ]; then
	DELTABD_SLICE=$(($MODEL_DELTABD-$MODEL_BACK))
	while [ $DELTABD_SLICE -lt 0 ]; do
	    DELTABD_SLICE=$(($DELTABD_SLICE+$MODEL_FREQINI_INPUT))
	done
	D3=`date_sub $DATE $TIME $MODEL_DELTABD`
	T3=`time_sub $DATE $TIME $MODEL_DELTABD`
    else
	DELTABD_SLICE=0
    fi
    DELTABD_SAVE=$DELTABD_SLICE
}


## @fn nwpbctimeloop_loop()
## @brief Advance a loop on the model runs providing input boundary conditions for a NWP run.
## @details This function has to be called for advancing a loop over
## the model runs that provide the boundary conditions to an
## assimilation or forecast run. The function sets the variables
## `$D2`, `$T2`, `$MODEL_START_SLICE`, `$MODEL_STOP_SLICE`,
## `$MODEL_FREQ_SLICE`, `$DELTABD_SLICE`. The return value is 1 (true)
## if the loop is not terminated, or 0 (false) if the loop is
## terminated, thus it should be used in the following way:
## 
##     nwpbctimeloop_init
##     while nwpbctimeloop_loop; do
##         performoperationsintheloop
##     done
nwpbctimeloop_loop() {
    [ $MODEL_STOP_SLICE -lt $MODEL_STOP ] || return 1
    DELTABD_SLICE=$DELTABD_SAVE
    D2=`date_sub $D1 $T1 $DELTABD_SLICE`
    T2=`time_sub $D1 $T1 $DELTABD_SLICE`

    MODEL_START_SLICE=`max 0 $((-$DELTABD_SLICE))`
    if [ "$MODEL_BCANA" = "Y" ]; then
	MODEL_STOP_SLICE=$MODEL_START_SLICE
# prepare for next loop, update DELTABD_SAVE so that DELTABD_SLICE can be
# used outside
	DELTABD_SAVE=$(($DELTABD_SLICE-$MODEL_FREQANA_INPUT))
    else
# test whether this is the last possible loop
	DT=`date_add $D2 $T2 $MODEL_FREQINI_INPUT`
	TT=`time_add $D2 $T2 $MODEL_FREQINI_INPUT`
	if [ $DT$TT -gt $D3$T3 ]; then
	    MODEL_STOP_SLICE=$MODEL_STOP
	else
	    MODEL_STOP_SLICE=`min $MODEL_STOP $(($MODEL_FREQINI_INPUT-$DELTABD_SLICE-$MODEL_FREQFC_INPUT))`
	fi
# prepare for next loop, update DELTABD_SAVE so that DELTABD_SLICE can be
# used outside
	DELTABD_SAVE=$(($DELTABD_SLICE-$MODEL_FREQINI_INPUT))
    fi

# is this used anywhere? erase 
    if [ $MODEL_START_SLICE -eq 0 ]; then
	MODEL_LANA=.TRUE.
    else
	MODEL_LANA=.FALSE.
    fi

    return 0
}


# start exporting all assignments
set -a
check_dep nwptime
check_defined DATE TIME MODEL_BACK MODEL_DELTABD MODEL_STOP
# init timeloop
nwptime_init
# stop exporting all assignments
set +a

