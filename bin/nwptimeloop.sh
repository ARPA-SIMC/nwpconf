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

timeloop_init() {
# start of NWP run
    D1=`date_sub $DATE $TIME $NWP_BACK`
    T1=`time_sub $DATE $TIME $NWP_BACK`
    D2=$D1
    T2=$T1
# NWP_DELTABD is difference (hours) between $DATE$TIME (end of
# assimilation window / start of forecast) and start of last available
# input forecast providing BC (for BCANA=N)
    if [ "$NWP_BCANA" = "Y" ]; then
	export NWP_FREQ_INPUT=$NWP_FREQANA_INPUT
	NWP_DELTABD=0
    else
	export NWP_FREQ_INPUT=$NWP_FREQFC_INPUT
    fi
    DELTABD=0
    # DELTABD=$(($NWP_DELTABD-$NWP_BACK))
    # while [ $DELTABD -lt 0 ]; do
    # 	DELTABD=$(($DELTABD+$NWP_FREQANA_INPUT))
    # done
    # DELTABDLOCAL=$DELTABD
}

int2lm_loop_init() {
    NWP_FULL_STOP=$NWP_STOP
    NWP_STOP=0
# DELTABD is difference between start of assimilation and start of
# input forecast suitable for providing BC
    if [ "$NWP_BCANA" != "Y" ]; then
	DELTABD=$(($NWP_DELTABD-$NWP_BACK))
	while [ $DELTABD -lt 0 ]; do
	    DELTABD=$(($DELTABD+$NWP_FREQINI_INPUT))
	done
	D3=`date_sub $DATE $TIME $NWP_DELTABD`
	T3=`time_sub $DATE $TIME $NWP_DELTABD`
    fi
    DELTABDLOCAL=$DELTABD
}


int2lm_loop() {
    [ $NWP_STOP -lt $NWP_FULL_STOP ] || return 1
    DELTABD=$DELTABDLOCAL
    D2=`date_sub $D1 $T1 $DELTABD`
    T2=`time_sub $D1 $T1 $DELTABD`

    NWP_START=`max 0 $((-$DELTABD))`
    if [ "$NWP_BCANA" = "Y" ]; then
	NWP_STOP=$NWP_START
# prepare for next loop, update DELTABDLOCAL so that DELTABD can be
# used outside
	DELTABDLOCAL=$(($DELTABD-$NWP_FREQANA_INPUT))
    else
# test whether this is the last possible loop
	DT=`date_add $D2 $T2 $NWP_FREQINI_INPUT`
	TT=`time_add $D2 $T2 $NWP_FREQINI_INPUT`
	if [ $DT$TT -gt $D3$T3 ]; then
	    NWP_STOP=$NWP_FULL_STOP
	else
	    NWP_STOP=`min $NWP_FULL_STOP $(($NWP_FREQINI_INPUT-$DELTABD-$NWP_FREQFC_INPUT))`
	fi
# prepare for next loop, update DELTABDLOCAL so that DELTABD can be
# used outside
	DELTABDLOCAL=$(($DELTABD-$NWP_FREQINI_INPUT))
    fi

    if [ $NWP_START -eq 0 ]; then
	NWP_LANA=.TRUE.
    else
	NWP_LANA=.FALSE.
    fi

    return 0
}


# start exporting all assignments
set -a
# checks
# check_dep
check_defined DATE TIME NWP_BACK NWP_DELTABD NWP_FREQINI_INPUT NWP_STOP
# init timeloop
timeloop_init
# stop exporting all assignments
set +a

