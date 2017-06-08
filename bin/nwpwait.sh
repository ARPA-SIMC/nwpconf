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

## @details This module provides functions for intelligently waiting
## for the occurrence of specific events, such as the availability of
## input data.

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


nwpwait_wait() {
    if [ -n "$NWPWAITFINAL" ]; then
	if [ `date -u +%s` -gt $NWPWAITFINAL ]; then
	    echo "Final wait time reached, exiting"
	    return 1
	fi
    fi
    if [ -n "$NWPWAITWAIT" ]; then
	sleep $NWPWAITWAIT
    fi

}

# start exporting all assignments
#set -a
check_dep nwpwait
#check_defined NWPWAITWAIT
# stop exporting all assignments
#set +a

