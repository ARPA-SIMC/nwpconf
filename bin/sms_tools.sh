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


# Initialise the main SMS/ECFlow meter for the current process. That
# meter will be used for the subsequent operations. If the process is
# not running within the SMS environment, it will timeout after 10
# seconds.
# $1 the name of the meter
# $2 the optional initial value to be set, 0 if not provided
meter_init() {

    if [ -n "$SMSNAME" ]; then # running in sms
	export SMSMETER=$1
	if [ -n "$2" ]; then
            export SMSMETER_COUNT=$2
	else
            export SMSMETER_COUNT=0
	fi
	timeout_exec 10 smsmeter $SMSMETER $SMSMETER_COUNT || true
    fi
 
}

# Increment by one the main SMS/ECFlow meter for the current
# process. The sms_meter_init() function must have been called
# previously.
meter_increment() {

    if [ -n "$SMSMETER" ]; then
        [ -z "$SMSMETER_COUNT" ] && SMSMETER_COUNT=0
        SMSMETER_COUNT=$(($SMSMETER_COUNT + 1))
        timeout_exec 10 smsmeter $SMSMETER $SMSMETER_COUNT || true
    fi

}

set -a
# checks
check_dep sms_tools nwpconf
# stop exporting all assignments
set +a
