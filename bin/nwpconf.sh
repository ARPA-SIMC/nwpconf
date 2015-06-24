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
## package, it has to be sourced by the user script and
## other optional modules of this package have to be sourced after
## this.
## 
## This module provides tools for managing multiple configurations and
## generating configuration files from templates, tools for date and
## time computations and other utilities.
## 
## ### Managing multiple configurations
## 
## Configurations in nwpconf are managed in a hierarchical way, the
## user defines a directory tree rooted at `$NWPCONFDIR` and
## containing three more levels of subdirectories defined by the
## environmental variables exported in the main shell script, in order
## of growing priority:
## 
##     $NWPCONFDIR/
##     $NWPCONFDIR/$PROFILE/
##     $NWPCONFDIR/$PROFILE/$PROCESS/
##     $NWPCONFDIR/$PROFILE/$PROCESS/$PHASE/
## 
## ### Defining the configuration and importing it in the environment
## 
## Each of these directory levels may contain a shell script file
## `conf.sh`; upon sourcing of the current module, the configuration
## tree for the requested configuration is scanned and _all_ the
## `conf.sh` scripts encountered are sourced in order of growing
## priority. In this way, environment variable assignments made in
## following levels will override the ones made in the previous
## levels. The `conf.sh` files should mainly contain assignments of
## variable with shell syntax, however any arbitrary shell code in
## them will be regularly executed. If a directory level does not
## contain such a file, it will be skipped without errors. All the
## configuration files in the tree are sourced in automatic export
## mode, so that all variable assignments are exported even without
## the explicit `export` keyword. Thus, after sourcing the present
## script, the user environment will contain all the variable
## assignments according to the requested configurations.
## 
## ### Generating configuration files from templates
## 
## The configuration tree described above can also contain template
## files named `<filename>.in`. By means of the function
## conf_template(), the file `<filename>` will be generated in the
## current directory starting from the template file `<filename>.in`
## found in the configuration tree; unlike the case for `conf.sh`, if
## more than one matching template file is found in the tree, _only_
## the one contained in the directory with the highest priority is
## used. Any occurrence of `@<string>@` in the template file will be
## replaced by the current value of `$<string>` environment variable
## in the destination file.


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
## present module, so it is unlikely for it to be called directly by
## the user; it creates the list of directories to search for
## configuration files in, according to the user selected
## PROFILE/PROCESS/PHASE, ordered from lower to higher priority. It
## requires the variables `$PROFILE` `$PROCESS` `$PHASE` to be
## defined.
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

## @fn conf_template()
## @brief Generate file from template.

## @details This function generates one or more files from the
## corresponding template according to the current
## PROFILE/PROCESS/PHASE configuration. A template file with the same
## name and additional suffix `.in` must exist in the configuration
## tree, it will be used for generating the file in the current
## directory.
## @param $* list of files to be generated
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
    [ -n "$DATECOM" ] || DATECOM="date -u"
}

## @fn date_add()
## @brief Add the requested number of hours to a date and return the date.
## @details This function takes in input a date and time string, adds
## to it an integer number of hours and prints to standard output only
## the recomputed date in the format `YYYYMMDD`. All the computations
## are performed in UTC unless the variable `$DATECOM` is changed from
## its default value `date -u`.
## @param $1 initial date in the format `YYYYMMDD`
## @param $2 initial time in the format `HH`
## @param $3 number of hours to add, integer with __no leading zeroes__, negative values accepted
date_add() {
    $DATECOM --date "$1 $2:00 `signedhour_to_date $3`" '+%Y%m%d'
}

## @fn time_add()
## @brief Add the requested number of hours to a date and return the time.
## @details This function works as date_add() but it prints to
## standard output only the recomputed time in the format `HH`.
## @param $1 initial date in the format `YYYYMMDD`
## @param $2 initial time in the format `HH`
## @param $3 number of hours to add, integer with __no leading zeroes__, negative values accepted
time_add() {
    $DATECOM --date "$1 $2:00 `signedhour_to_date $3`" '+%H'
}

## @fn datetime_add()
## @brief Add the requested number of hours to a date and return date and time.
## @details This function works as date_add() but it prints to
## standard output the recomputed date and time in the format `YYYYMMDDHH`.
## @param $1 initial date in the format `YYYYMMDD`
## @param $2 initial time in the format `HH`
## @param $3 number of hours to add, integer with __no leading zeroes__, negative values accepted
datetime_add() {
    $DATECOM --date "$1 $2:00 `signedhour_to_date $3`" '+%Y%m%d%H'
}

## @fn date_sub()
## @brief Subtract the requested number of hours to a date and return the date.
## @details This function works as date_add() but it subtracts hours
## rather then adding them.
## @param $1 initial date in the format `YYYYMMDD`
## @param $2 initial time in the format `HH`
## @param $3 number of hours to subtract, integer with __no leading zeroes__, negative values accepted
date_sub() {
    $DATECOM --date "$1 $2:00 `minus_signedhour_to_date $3`" '+%Y%m%d'
}

## @fn time_sub()
## @brief Subtract the requested number of hours to a date and return the time.
## @details This function works as time_add() but it subtracts hours
## rather then adding them.
## @param $1 initial date in the format `YYYYMMDD`
## @param $2 initial time in the format `HH`
## @param $3 number of hours to subtract, integer with __no leading zeroes__, negative values accepted
time_sub() {
    $DATECOM --date "$1 $2:00 `minus_signedhour_to_date $3`" '+%H'
}

## @fn datetime_sub()
## @brief Subtract the requested number of hours to a date and return date and time.
## @details This function works as datetime_add() but it subtracts hours
## rather then adding them.
## @param $1 initial date in the format `YYYYMMDD`
## @param $2 initial time in the format `HH`
## @param $3 number of hours to subtract, integer with __no leading zeroes__, negative values accepted
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

