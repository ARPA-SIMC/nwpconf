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
## @brief Module with functions related to waiting for events.

## @details This module provides functions for configuring wait
## cycles. The environmental variables that can be preliminarly set
## are \a $NWPWAITSOLAR and \a $NWPWAITELAPS ; the first sets a
## maximum relative wait time in seconds with respect to \a
## $DATE$TIME, while the second sets a maximum cumulative wait
## time. If both variables are set the maximum wait time is the
## minimum of the two times; if none of the two is set the maximum
## wait time is infinite.

## @fn check_run()
## @brief Check whether a process can run.
## @details this function checks whether \a $NWPWAITSOLAR_RUN seconds
## have elapsed since $\a $DATE$TIME and returns with a code 1
## (error) if this is not true. If any of the required variables is not
## set, it returns with a code 0.
## It is a preliminary check which does not interfere with the other 
## functions of this module.
check_run() {
    local wait=

    if [ -n "$NWPWAITSOLAR_RUN" -a -n "$DATE" -a -n "$TIME" ]; then
        wait=$((`date -u --date "$DATE $TIME" +%s` + $NWPWAITSOLAR_RUN))
        if [ `date -u +%s` -lt $wait ]; then
            return 1
        fi
    fi
}


## @fn wait_run()
## @brief Wait until a process can run.
## @details this function waits until \a $NWPWAITSOLAR_RUN seconds
## have elapsed since $\a $DATE$TIME .
## If any of the required variables is not set, it returns immediately
## with a code 0.
## It is a preliminary check which does not interfere with the other 
## functions of this module.
wait_run() {
    local wait=

    if [ -n "$NWPWAITSOLAR_RUN" -a -n "$DATE" -a -n "$TIME" ]; then
        wait=$((`date -u --date "$DATE $TIME" +%s` + $NWPWAITSOLAR_RUN - `date -u +%s`))
        if [ "$wait" -gt 0 ]; then
            sleep $wait
        fi
    fi
}


## @fn nwpwait_setup()
## @brief Setup the wait system.
## @details this function has to be called once at the beginning of
## the program in order to set the variables required for the wait
## functions.
nwpwait_setup() {
    local wait=
    NWPWAITFINAL=

    if [ -n "$NWPWAITELAPS" ]; then
	NWPWAITFINAL=$((`date -u +%s` + $NWPWAITELAPS))
    fi
    if [ -n "$NWPWAITSOLAR" -a -n "$DATE" -a -n "$TIME" ]; then
	wait=$((`date -u --date "$DATE $TIME" +%s` + $NWPWAITSOLAR))
	if [ -n "$NWPWAITFINAL" ]; then
	    NWPWAITFINAL=`min $wait $NWPWAITFINAL`
	else
	    NWPWAITFINAL=$wait
	fi
    fi

}


## @fn nwpwait_wait()
## @brief Perform a wait cycle
## @details This function checks if the maximum wait time has been
## reached; if so it returns with a code 1 (error), while, if it is not the
## case, it waits \a $NWPWAITWAIT seconds and returns 0 exit code.
nwpwait_wait() {
    nwpwait_check || return 1
    if [ -n "$NWPWAITWAIT" ]; then
	sleep $NWPWAITWAIT
    fi

}


## @fn nwpwait_check()
## @brief Check for final time
## @details This function just checks if the maximum wait time has
## been reached and returns with a code 1 (error), if it is the case.
nwpwait_check() {
    if [ -n "$NWPWAITFINAL" ]; then
	if [ `date -u +%s` -gt $NWPWAITFINAL ]; then
	    echo "Final wait time reached, exiting"
	    return 1
	fi
    fi
}


# start exporting all assignments
#set -a
check_dep nwpwait
#check_defined NWPWAITWAIT
# stop exporting all assignments
#set +a

