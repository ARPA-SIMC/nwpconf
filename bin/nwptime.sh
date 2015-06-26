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
	export MODEL_FREQ_INPUT=$MODEL_FREQANA_INPUT
	MODEL_DELTABD=0
    else
	export MODEL_FREQ_INPUT=$MODEL_FREQFC_INPUT
    fi
    DELTABD=0
    # DELTABD=$(($MODEL_DELTABD-$MODEL_BACK))
    # while [ $DELTABD -lt 0 ]; do
    # 	DELTABD=$(($DELTABD+$MODEL_FREQANA_INPUT))
    # done
    # DELTABDLOCAL=$DELTABD
}


## @fn nwpbctimeloop_init()
## @brief Initialise a loop on the model runs providing input boundary conditions for a NWP run.
## @details This function has to be called for initialising a loop
## over the model runs that provide the boundary conditions to an
## assimilation run. The most common cases are taken into account,
## like using analysed or forecast BC's, possibly with a shift in time
## and with specified frequency of availability.
nwpbctimeloop_init() {
    MODEL_FULL_STOP=$MODEL_STOP
    MODEL_STOP=0
# DELTABD is difference between start of assimilation and start of
# input forecast suitable for providing BC
    if [ "$MODEL_BCANA" != "Y" ]; then
	DELTABD=$(($MODEL_DELTABD-$MODEL_BACK))
	while [ $DELTABD -lt 0 ]; do
	    DELTABD=$(($DELTABD+$MODEL_FREQINI_INPUT))
	done
	D3=`date_sub $DATE $TIME $MODEL_DELTABD`
	T3=`time_sub $DATE $TIME $MODEL_DELTABD`
    fi
    DELTABDLOCAL=$DELTABD
}


## @fn nwpbctimeloop_loop()
## @brief Advance a loop on the model runs providing input boundary conditions for a NWP run.
## @details This function has to be called for advancing a loop over
## the model runs that provide the boundary conditions to an
## assimilation run. The function sets the variables `$D2`, `$T2`,
## `$MODEL_START`, `$MODEL_STOP`, `$DELTABD` The return value is 1
## (true) if the loop is not finished or 0 (false) if the loop is
## finished, thus it should be used in the following way:
## 
##     nwpbctimeloop_init
##     while nwpbctimeloop_loop; do
##         performoperationsintheloop
##     done
nwpbctimeloop_loop() {
    [ $MODEL_STOP -lt $MODEL_FULL_STOP ] || return 1
    DELTABD=$DELTABDLOCAL
    D2=`date_sub $D1 $T1 $DELTABD`
    T2=`time_sub $D1 $T1 $DELTABD`

    MODEL_START=`max 0 $((-$DELTABD))`
    if [ "$MODEL_BCANA" = "Y" ]; then
	MODEL_STOP=$MODEL_START
# prepare for next loop, update DELTABDLOCAL so that DELTABD can be
# used outside
	DELTABDLOCAL=$(($DELTABD-$MODEL_FREQANA_INPUT))
    else
# test whether this is the last possible loop
	DT=`date_add $D2 $T2 $MODEL_FREQINI_INPUT`
	TT=`time_add $D2 $T2 $MODEL_FREQINI_INPUT`
	if [ $DT$TT -gt $D3$T3 ]; then
	    MODEL_STOP=$MODEL_FULL_STOP
	else
	    MODEL_STOP=`min $MODEL_FULL_STOP $(($MODEL_FREQINI_INPUT-$DELTABD-$MODEL_FREQFC_INPUT))`
	fi
# prepare for next loop, update DELTABDLOCAL so that DELTABD can be
# used outside
	DELTABDLOCAL=$(($DELTABD-$MODEL_FREQINI_INPUT))
    fi

    if [ $MODEL_START -eq 0 ]; then
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

