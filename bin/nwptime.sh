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
## - `$DATE` (YYYYMMDD) and `$TIME` (hhmm) indicate the current
##   nominal date and time, i.e. the start of the run for a forecast
##   run or the end of the run for an assimilation run
## 
## - `$MODEL_STOP` indicates the total absolute duration of the run in
##   hours
## 
## - `$MODEL_BACK` indicates the number of hours to go back from
##   `$DATE$TIME` for starting an assimilation or warm-up run, typical
##   values are 0 for a forecast run and `$MODEL_BACK` for
##   assimilation or warm-up, intermediate values between 0 and
##   `$MODEL_BACK` may give unexpected results
## 
## - `$MODEL_BCANA` if set to `Y` indicates that analysis data are
##   used as boundary conditions
## 
## - `$MODEL_DELTABD` indicates the difference in hours between
##   `$DATE$TIME` and the initialisation time of last available input
##   model forecast, it is ignored if `$MODEL_BCANA == Y`
## 
## - `$PARENTMODEL_FREQANA` indicates the interval in hours between
##   available analysis data from the driving model, used in the case
##   of `$MODEL_BCANA == Y`, it is assumed that analyses are produced
##   at 00 UTC and successively every `$PARENTMODEL_FREQANA` hours
## 
## - `$PARENTMODEL_FREQINI` indicates the interval in hours between
##   initialisation of successive driving model forecasts, used in the
##   case of `$MODEL_BCANA != Y`, it is assumed that forecasts are
##   initialised at 00 UTC and successively every
##   `$PARENTMODEL_FREQINI` hours
## 
## - `$PARENTMODEL_FREQFC` indicates the interval in hours between
##   successive driving model forecast steps of a single forecast run,
##   used in the case of `$MODEL_BCANA != Y`
## 
## It is an optional module and it has to be sourced after the main
## _nwpconf.sh_ module.


## @fn nwptime_init()
## @brief Initialise the time-related environment for a NWP run.
## @details This function is implicitly called when the module is
## sourced, it sets the following variables:
## 
## - `$DATES` and `$TIMES` absolute date and time of start of model
##   run, either assimilation or forecast, based on `$DATE`, `$TIME`
##   and `$MODEL_BACK`
## 
## - `$DATEE` and `$TIMEE` absolute date and time of end of model
##   run, either assimilation or forecast, based on `$DATE`, `$TIME`,
##   `$MODEL_BACK` and `$MODEL_STOP`
nwptime_init() {
# start of NWP run
    DATES=`date_sub $DATE $TIME $MODEL_BACK`
    TIMES=`time_sub $DATE $TIME $MODEL_BACK`
    DATEE=`date_add $DATES $TIMES $MODEL_STOP`
    TIMEE=`time_add $DATES $TIMES $MODEL_STOP`
# reset MODEL_DELTABD when bcana is set
    if [ "$MODEL_BCANA" = "Y" ]; then
	MODEL_DELTABD=0
    fi
}


## @fn nwpbctimeloop_init()
## @brief Initialise a loop on the model runs providing input boundary conditions for a NWP run.
## @details This function has to be called for initialising a loop
## over the input model runs (called "slices") that provide the
## boundary conditions to an assimilation run. The most common cases
## are taken into account, like using analysed or forecast BC's,
## possibly with a shift in time and with specified frequency of
## availability. It sets the following variables:
## 
## - `$MODEL_FREQ_SLICE` interval in h between available BCs from
##   first slice providing BC (actually equal for every slice)
## 
## For the correct use see _nwpbctimeloop_loop()_.
nwpbctimeloop_init() {
    MODEL_STOP_SLICE=0
    : ${MODEL_BACK:=0} # set to 0 if unset
# MODEL_DELTABD_SLICE is difference between start of assimilation and start of
# input forecast suitable for providing BC
    if [ "$MODEL_BCANA" != "Y" ]; then
	MODEL_DELTABD_SLICE=$(($MODEL_DELTABD-$MODEL_BACK))
	while [ $MODEL_DELTABD_SLICE -lt 0 ]; do
	    MODEL_DELTABD_SLICE=$(($MODEL_DELTABD_SLICE+$PARENTMODEL_FREQINI))
	done
	D3=`date_sub $DATE $TIME $MODEL_DELTABD`
	T3=`time_sub $DATE $TIME $MODEL_DELTABD`
	export MODEL_FREQ_SLICE=$PARENTMODEL_FREQFC
    else
	export MODEL_FREQ_SLICE=$PARENTMODEL_FREQANA
	MODEL_DELTABD_SLICE=0
    fi
    DELTABD_SAVE=$MODEL_DELTABD_SLICE
}


## @fn nwpbctimeloop_loop()
## @brief Advance a loop on the model runs providing input boundary conditions for a NWP run.
## @details This function has to be called for advancing a loop over
## the model runs (called "slices") that provide the boundary
## conditions to an assimilation or forecast run. The function sets
## the variables:
## 
## - `$DATES_SLICE` and `$TIMES_SLICE` date and time of start of current
##   slice of input model providing BCs
## 
## - `$MODEL_START_SLICE` and `$MODEL_STOP_SLICE` indicate the first
##   and the last instants, in h starting from `$DATES` and `$TIMES`,
##   for which BCs are taken from current slice
## 
## - `$MODEL_DELTABD_SLICE` difference in h between start of assimilation
##    and start of current slice of input model providing BCs
## 
## - `$MODEL_INI_SLICE` whether current slice of input model may provide
##   also initial data (`.TRUE.`) or only boundary data (`.FALSE.`).
## 
## The return value is 1 (true) if the loop is not terminated, or 0
## (false) if the loop is terminated, thus it should be used in the
## following way:
## 
##     nwpbctimeloop_init
##     while nwpbctimeloop_loop; do
##         performoperationsintheloop
##     done
nwpbctimeloop_loop() {
    [ $MODEL_STOP_SLICE -lt $MODEL_STOP ] || return 1
    MODEL_DELTABD_SLICE=$DELTABD_SAVE
    DATES_SLICE=`date_sub $DATES $TIMES $MODEL_DELTABD_SLICE`
    TIMES_SLICE=`time_sub $DATES $TIMES $MODEL_DELTABD_SLICE`

    MODEL_START_SLICE=`max 0 $((-$MODEL_DELTABD_SLICE))`
    if [ "$MODEL_BCANA" = "Y" ]; then
	MODEL_STOP_SLICE=$MODEL_START_SLICE
# prepare for next loop, update DELTABD_SAVE so that MODEL_DELTABD_SLICE can be
# used outside
	DELTABD_SAVE=$(($MODEL_DELTABD_SLICE-$PARENTMODEL_FREQANA))
    else
# test whether this is the last possible loop
	DT=`date_add $DATES_SLICE $TIMES_SLICE $PARENTMODEL_FREQINI`
	TT=`time_add $DATES_SLICE $TIMES_SLICE $PARENTMODEL_FREQINI`
	if [ $DT$TT -gt $D3$T3 ]; then
	    MODEL_STOP_SLICE=$MODEL_STOP
	else
	    MODEL_STOP_SLICE=`min $MODEL_STOP $(($PARENTMODEL_FREQINI-$MODEL_DELTABD_SLICE-$PARENTMODEL_FREQFC))`
	fi
# prepare for next loop, update DELTABD_SAVE so that MODEL_DELTABD_SLICE can be
# used outside
	DELTABD_SAVE=$(($MODEL_DELTABD_SLICE-$PARENTMODEL_FREQINI))
    fi

# used in COSMO namelist linitial
    if [ $MODEL_START_SLICE -eq 0 ]; then
	MODEL_INI_SLICE=.TRUE.
    else
	MODEL_INI_SLICE=.FALSE.
    fi

    return 0
}


# start exporting all assignments
set -a
check_dep nwptime
check_defined DATE TIME MODEL_BACK MODEL_DELTABD MODEL_STOP
# init module
nwptime_init
# stop exporting all assignments
set +a

