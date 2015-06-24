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


# @author Davide Cesari
# @copyright GPLv3
# @version 1.0
## @file
## @brief Main entry point for the NWPconf bash library.
## @details The file nwpconf.sh is the main entry point for the whole
## package, it has to be compulsorily sourced by the user script and
## other optional modules of this package have to be sourced after
## this.
##
## This module provides tools for managing sets of configuration
## files, date and time computations and other utilities.
##
## Configuration files are managed in a hierarchical way, the user
## defines a directory tree rooted at `$NWPCONFDIR` and containing 3
## level of subdirectories defined by the environmental variables
## exportd in the main shell script, as
## `$NWPCONFDIR/$PROFILE/$PROCESS/$PHASE`. Each of these directory may
## contain a shel script file `conf.sh`; upon sourcing of the current
## module, the configuration tree for the requested configuration is
## scanned and all the `conf.sh` scripts encountered are sourced
## starting from the root in the following order:
##
##     $NWPCONFDIR/conf.sh
##     $NWPCONFDIR/$PROFILE/conf.sh
##     $NWPCONFDIR/$PROFILE/$PROCESS/conf.sh
##     $NWPCONFDIR/$PROFILE/$PROCESS/$PHASE/conf.sh
##
## Thus environmental variable assignments made in following levels
## will override those made in the previous ones. The scripts should
## mainly contain variable assignments although regular shell code in
## them will be executed as well. If a directory level does not
## contain such a script, it will be skipped without errors. All the
## scripts in the configuration tree are sourced in automatic export
## mode, thus all variable assignments are exported even without the
## `export keyword`.

# Add current file to the list of loaded modules and check for
# optional dependencies
# $* optional list of modules (without .sh suffix) on which this module depends
check_dep() {
#    set|grep nwpconf
#    NWPCONFLIST="$NWPCONFLIST `basename $0 .sh`"
# $0 does not work, try ${BASH_SOURCE[0]}?
# check if each of $@ are in list
#    local dep
#    for dep in $@; do
#	if
}

# Check if a list of variables is defined
check_defined() {
    local var
    local val
    local err=0
    for var in $@; do
        val=`eval echo '$'$var`
        if [ -z "$val" ]; then
	    echo "Variable \$$var must be defined"
	    err=1
        fi
    done
    return $err
}

# internal function
_confdirlist_add() {
    if [ -d $1 ]; then
	confdirlist="$confdirlist $1"
    fi
}

## @fn conf_init()
## @brief Create a list of configuration directories for current configuration
## @details This function is implicitly called when sourcing the
## present module, it creates the list of directory to search for
## configuration files for current PROFILE/PROCESS/PHASE ordered from
## lower to higher priority. It requires the variables `$PROFILE`
## `$PROCESS` `$PHASE` to be defined.
conf_init() {
    confdirlist=''
    _confdirlist_add $NWPCONFDIR
    for prof in $PROFILE; do
	_confdirlist_add $NWPCONFDIR/$prof
	if [ -n "$PROCESS" ]; then
	    _confdirlist_add $NWPCONFDIR/$prof/$PROCESS
	    if [ -n "$PHASE" ]; then
		_confdirlist_add $NWPCONFDIR/$prof/$PROCESS/$PHASE
	    fi
	fi
    done
}

# Source configuration from the already created list of directories
conf_source() {
    for dir in $confdirlist; do
	if [ -f "$dir/conf.sh" ]; then
	    source "$dir/conf.sh"
	fi
    done
}

# Output on stdout the path of the requested file according to current
# configuration.
# $1 the name of the requested file
conf_getfile() {
    local conffile=''
    for dir in $confdirlist; do
	if [ -f "$dir/$1" ]; then
	    conffile="$dir/$1"
	fi
    done
    echo $conffile
}

# Generate one or more files from the corresponding template according
# to current configuration. A template file with the same name and
# additional suffix .in must exist in the configuration tree, it will
# be used for generating the file in the current directory.
# $* a list of files to be generated
conf_template() {
    for file in $*; do
	template=`conf_getfile $file.in`
	if [ -n $template ]; then
	    $NWPCONFBINDIR/ac_templater.py $template > $file
	fi
    done
}

# Setup date functions
date_init() {
    [ -n "$DATECOM" ] || DATECOM=date
}

# functions for returning on stdout date or time of day (hours) or
# both incremented or decremented by the requested amount of hours
# arguments are date (YYYYMMDD), time (HH) and increment in hours

date_add() {
    $DATECOM --date "$1 $2:00 `signedhour_to_date $3`" '+%Y%m%d'
}

time_add() {
    $DATECOM --date "$1 $2:00 `signedhour_to_date $3`" '+%H'
}

datetime_add() {
    $DATECOM --date "$1 $2:00 `signedhour_to_date $3`" '+%Y%m%d%H'
}

date_sub() {
    $DATECOM --date "$1 $2:00 `minus_signedhour_to_date $3`" '+%Y%m%d'
}

time_sub() {
    $DATECOM --date "$1 $2:00 `minus_signedhour_to_date $3`" '+%H'
}

datetime_sub() {
    $DATECOM --date "$1 $2:00 `minus_signedhour_to_date $3`" '+%Y%m%d%H'
}

signedhour_to_date() {
    if [ "$1" -lt 0 ]; then
	echo "$((-$1)) hours ago"
    else
	echo "$1 hours"
    fi
}

minus_signedhour_to_date() {
    if [ "$1" -lt 0 ]; then
	echo "$((-$1)) hours"
    else
	echo "$1 hours ago"
    fi
}

# The date and time as requested by reftime arki-query key
datetime_arki() {
    $DATECOM --date "$1 $2:00" '+%Y-%m-%d %H:00'
}

# Delta time to be used in COSMO grib file names, input forecast time
# in h, output ddhh0000
timedelta_cosmo() {
    local d=0
    local h=$1
    while [ "$h" -ge 24 ]; do
	d=$(($d+1))
	h=$(($h-24))
    done
    printf "%02d%02d0000\n" $d $h
    
}

## @fn max()
## @brief Compute the maximum between two numerical arguments.
## @details This function computes the maximum between the two integer
## numerical arguments provided and prints it to stdout, thus it
## should tipically be used as: ``M=`max $H1 $H2```.
##
## @param $1 the first numerical argument
## @param $2 the second numerical argument
max() {
    if [ "$1" -gt "$2" ]; then
	echo $1
    else
	echo $2
    fi
}

min() {
    if [ "$1" -lt "$2" ]; then
	echo $1
    else
	echo $2
    fi
}


# logsim wait function
wait_logsim() {

    while true; do
# \\pset format unaligned \\\\
	if [ -n "$1" ]; then
	    n=`echo "SELECT count(*) FROM imports i, entities e WHERE i.entity_id = e.id AND message = '$1' AND reftime = timestamp with time zone '$2:00+00' AND name = '$3';" \
		| psql -h log.metarpa -d simclogdb -U simclog -A -F ',' -n -q -t`
	else
	    n=`echo "SELECT count(*) FROM imports i, entities e WHERE i.entity_id = e.id AND reftime = timestamp with time zone '$2:00+00' AND name = '$3';" \
		| psql -h log.metarpa -d simclogdb -U simclog -A -F ',' -n -q -t`
	fi
	if [ "$n" -ge 1 ]; then
	    return 0
	fi
	sleep 60
    done
# password in ~/.pgpass
}

## @fn timeout_exec()
## @brief Execute a command with a specified timeout.
## @details This function executes an arbitrary command killing it
## when the requested time has elapsed.
## @param $1 value of the timeout in seconds
## @param $* command to be executed and optional extra-arguments
timeout_exec() {
    local SIG=-TERM # signal sent to the process when the timer expires
    local interval=5 # interval between checks if the process is still alive
    local delay=4 # delay between posting the given signal and destroying the process (kill -KILL)
    timeout=$1
    shift

    (
	for t in $timeout $delay
	do
	    while (( $t > $interval ))
	    do
		sleep $interval
		kill -0 $$ || exit
		t=$(( $t - $interval ))
	    done
	    sleep $t
	    kill $SIG $$ && kill -0 $$ || exit
	    SIG=-KILL
	done
    ) 2> /dev/null &

    exec "$@"
}


# start exporting all assignments
set -a
# checks
# check_dep
check_defined NWPCONFDIR NWPCONFBINDIR PROFILE PROCESS PHASE
# create confdirlist
conf_init
# import configuration
conf_source
# init date functions
date_init
# stop exporting all assignments
set +a

