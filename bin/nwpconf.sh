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
## user defines a common directory tree rooted at `$NWPCONFDIR`
## containing different branches for different processes to be
## configured. The configuration for the current process is defined by
## the `$NWPCONF` variable, thus all the directory levels between
## `$NWPCONFDIR` and `$NWPCONFDIR/$NPWCONF` define the configuration
## for the current process, with growing priority from `$NWPCONFDIR`
## to `$NWPCONFDIR/$NPWCONF`.
## 
## When the `$ENS_MEMB` variable is set to a nonempty value, the
## configuration is said to be running in ensemble mode, providing
## extra features for managing ensemble member-specific configurations
## and files.
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
## When running in ensemble mode, in every directory of the tree a
## file named `conf.sh.$ENS_MEMB` is searched for and sourced if
## found, after the corresponding `conf.sh` file, thus allowing to
## define an ensemble member-specific configuration.
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
## in the destination file. Any occurrence of `@INCLUDE <file>@` in
## the template file determines the inclusion of `<file>` in that
## point, applying recursively the same variable substitutions and
## file inclusion rules. If `<file>` contains a directory
## specification, either relative or absolute, it is searched starting
## from the current directory, otherwise it is searchd in the
## configuration tree with the same rules used for finding the
## template file.
## 
## When running in ensemble mode, in every directory of the tree,
## files named `<filename>.in.ens` and `<filename>.in.$ENS_MEMB` are
## also searched for, with increasing priority and with a higher
## priority than the corresponding `<filename>.in`.
## 
## ### Picking different variants of a generic file
## 
## The configuration tree can also contain generic files ready for use,
## at different configuration levels, the function conf_getfile() helps
## in choosing the one with highest priority in these cases.
## 
## As indicated above for template files, here too, in ensemble mode,
## the name of a file with an additional extension `.ens` (for any
## member, with lower priority) or `.$ENS_MEMB` (for a specific
## member, with higher priority), if present, is returned in place of
## the corresponding file without additional extension.

# Add current file to the list of loaded modules and check for
# optional dependencies
# $1 name of current module (file without .sh, hard to extract automatically whem sourced)
# $* optional list of modules (without .sh suffix) on which this module depends
check_dep() {
    eval export ${1}_LOADED=1
    shift
    local err=0 loaded
    for dep in $@; do
	loaded=`eval echo '$'${dep}_LOADED`
	if [ -z "$loaded" ]; then
	    echo "Error: module $dep.sh must be loaded before sourcing current module"
	    err=1
        fi
    done
    return $err
}

# Check whether a list of variables is defined
check_defined() {
    local var
    local val
    local err=0
    for var in $@; do
        val=`eval echo '$'$var`
        if [ -z "$val" ]; then
	    echo "Error: variable \$$var must be defined before sourcing current module"
	    err=1
        fi
    done
    return $err
}

# internal function
_confdirlist_add() {
    curdir="$curdir${curdir:+/}$1"
    if [ -d "$curdir" ]; then
	confdirlist="$confdirlist${confdirlist:+ }$curdir"
    else
	echo "configuration directory $curdir does not exist"
	return 1
    fi
}

## @fn conf_init()
## @brief Create a list of configuration directories for current configuration
## @details This function is implicitly called when sourcing the
## present module, so it is unlikely for it to be called directly by
## the user; it creates the list of directories to search for
## configuration files in, according to the user selected
## configuration `$NWPCONF` and the configuration root `$NWPCONFDIR`,
## ordered from lower to higher priority.
conf_init() {
    local curdir=''
    confdirlist=''
    _confdirlist_add $NWPCONFDIR

    local save_IFS=$IFS
    IFS=/
    local conf=($NWPCONF)
    IFS=$save_IFS
    for dir in ${conf[*]}; do
	_confdirlist_add $dir
    done
}

# Source configuration from the already created list of directories
conf_source() {
    for dir in $confdirlist; do
	if [ -f "$dir/conf.sh" ]; then
	    source "$dir/conf.sh"
	fi
	if [ -n "$ENS_MEMB" ]; then
	    if [ -f "$dir/conf.sh.ens" ]; then
		source "$dir/conf.sh.ens"
	    fi
	    if [ -f "$dir/conf.sh.$ENS_MEMB" ]; then
		source "$dir/conf.sh.$ENS_MEMB"
	    fi
	fi
    done
}

## @fn conf_getfile()
## @brief Return the path of the requested file according to current configuration
## @details This functions prints on stdout the name of the requested
## configuration file contained in the configuration tree defined by
## `$NWPCONFDIR` and `$NWPCONF`. If more than one file with the
## requested name is found, only the one with highest priority is
## returned, if no files are found the function returns with an error
## code.
## @param $1 the name of the requested file
conf_getfile() {
    local conffile=''
    for dir in $confdirlist; do
	if [ -f "$dir/$1" ]; then
	    conffile="$dir/$1"
	fi
	if [ -n "$ENS_MEMB" ]; then
	    if [ -f "$dir/$1.ens" ]; then
		conffile="$dir/$1.ens"
	    fi
	    if [ -f "$dir/$1.$ENS_MEMB" ]; then
		conffile="$dir/$1.$ENS_MEMB"
	    fi
	fi
    done
    if [ -n "$conffile" ]; then
	echo $conffile
    else
	return 1
    fi
}

## @fn conf_template()
## @brief Generate file from template.
## @details This function generates one or more files from the
## corresponding template according to the current configuration tree
## `$NWPCONF`. One or more template files with the same name and
## additional suffix `.in` must exist in the configuration tree, the
## one with highest priority will be used for generating the file in
## the current directory, if no template file is found the function
## returns with an error code.
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
    if [ "${#1}" -gt 8 ]; then
	$DATECOM --date "${1:0:8} ${1:8:4} `signedhour_to_date $2`" '+%Y%m%d'
    else
	$DATECOM --date "$1 $2:00 `signedhour_to_date $3`" '+%Y%m%d'
    fi
}

## @fn time_add()
## @brief Add the requested number of hours to a date and return the time.
## @details This function works as date_add() but it prints to
## standard output only the recomputed time in the format `HH`.
## @param $1 initial date in the format `YYYYMMDD`
## @param $2 initial time in the format `HH`
## @param $3 number of hours to add, integer with __no leading zeroes__, negative values accepted
time_add() {
    if [ "${#1}" -gt 8 ]; then
	$DATECOM --date "${1:0:8} ${1:8:4} `signedhour_to_date $2`" '+%H'
    else
	$DATECOM --date "$1 $2:00 `signedhour_to_date $3`" '+%H'
    fi
}

## @fn datetime_add()
## @brief Add the requested number of hours to a date and return date and time.
## @details This function works as date_add() but it prints to
## standard output the recomputed date and time in the format `YYYYMMDDHH`.
## @param $1 initial date in the format `YYYYMMDD`
## @param $2 initial time in the format `HH`
## @param $3 number of hours to add, integer with __no leading zeroes__, negative values accepted
datetime_add() {
    if [ "${#1}" -gt 8 ]; then
	$DATECOM --date "${1:0:8} ${1:8:4} `signedhour_to_date $2`" '+%Y%m%d%H'
    else
	$DATECOM --date "$1 $2:00 `signedhour_to_date $3`" '+%Y%m%d%H'
    fi
}

## @fn date_sub()
## @brief Subtract the requested number of hours to a date and return the date.
## @details This function works as date_add() but it subtracts hours
## rather then adding them.
## @param $1 initial date in the format `YYYYMMDD`
## @param $2 initial time in the format `HH`
## @param $3 number of hours to subtract, integer with __no leading zeroes__, negative values accepted
date_sub() {
    if [ "${#1}" -gt 8 ]; then
	$DATECOM --date "${1:0:8} ${1:8:4} `minus_signedhour_to_date $2`" '+%Y%m%d'
    else
	$DATECOM --date "$1 $2:00 `minus_signedhour_to_date $3`" '+%Y%m%d'
    fi
}

## @fn time_sub()
## @brief Subtract the requested number of hours to a date and return the time.
## @details This function works as time_add() but it subtracts hours
## rather then adding them.
## @param $1 initial date in the format `YYYYMMDD`
## @param $2 initial time in the format `HH`
## @param $3 number of hours to subtract, integer with __no leading zeroes__, negative values accepted
time_sub() {
    if [ "${#1}" -gt 8 ]; then
	$DATECOM --date "${1:0:8} ${1:8:4} `minus_signedhour_to_date $2`" '+%H'
    else
	$DATECOM --date "$1 $2:00 `minus_signedhour_to_date $3`" '+%H'
    fi
}

## @fn datetime_sub()
## @brief Subtract the requested number of hours to a date and return date and time.
## @details This function works as datetime_add() but it subtracts hours
## rather then adding them.
## @param $1 initial date in the format `YYYYMMDD`
## @param $2 initial time in the format `HH`
## @param $3 number of hours to subtract, integer with __no leading zeroes__, negative values accepted
datetime_sub() {
    if [ "${#1}" -gt 8 ]; then
	$DATECOM --date "${1:0:8} ${1:8:4} `minus_signedhour_to_date $2`" '+%Y%m%d%H'
    else
	$DATECOM --date "$1 $2:00 `minus_signedhour_to_date $3`" '+%Y%m%d%H'
    fi
}

## @fn date_now()
## @brief Return the current date.
## @details This function prints on stdout the current date in the
## format `YYYYMMDD`; the date is expressed in UTC unless the variable
## `$DATECOM` is changed from its default value `date -u`.
date_now() {
    $DATECOM '+%Y%m%d'
}

## @fn time_now()
## @brief Return the current time.
## @details This function works as date_now() but it prints the current time
## in the format `HH`.
time_now() {
    $DATECOM '+%H'
}

## @fn datetime_now()
## @brief Return the current time.
## @details This function works as date_now() but it prints the current date
## and time in the format `YYYYMMDDHH`.
datetime_now() {
    $DATECOM '+%Y%m%d%H'
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

## @fn datetime_cnmc()
## @brief Output the provided date and time in the format required by CNMC.
## @details This function prints to standard output the provided date
## and time in the format `MMDDHHMM` as used in the files provided by
## the Italian Military Meteorological Service (cnmc in WMO
## abbreviations).
## @param $1 date in the format `YYYYMMDD`
## @param $2 time in the format `HH`
datetime_cnmc() {
    if [ "${#1}" -gt 8 ]; then
	$DATECOM --date "${1:0:8} ${1:8:4}" '+%m%d%H%M'
    else
	$DATECOM --date "$1 $2:00" '+%m%d%H%M'
    fi
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
	for t in $timeout $delay; do
	    while [ "$t" -gt "$interval" ]; do
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

## @fn nonunique_exit
## @brief exit if another instance is currently running.
## @details If another instance of the same process, i.e. a script
## with the same file name (regardless of the absolute path), is
## already running under the same user id, the process exits.
nonunique_exit() {

    local comm=`basename $0`
    ps -u $USER -o pid= -o comm= | grep -v "^ *$$ " | \
	grep ${comm::15} > /dev/null && exit 0 || true
# is 15 univeral truncation of comm format? field length does not help
}

## @fn safe_rm_rf()
## @brief Remove recursively directory tree(s) with some security checks.
## @details This function removes the requested directories and all
## their content recursively (`rm -rf`) performing some preliminary
## checks to prevent removing undesired files due, e.g., to wrong
## environmental variable assignment. Each argument provided must
## match the following conditions:
## 
##  * being non null
##  * being not equal to /
##  * being a directory
##  * being owned by the user
## 
## otherwise the removal for the current argument is canceled and the
## following argument is examined.
## 
## @param $* directories to be removed
safe_rm_rf() {
    for dir in "$@"; do
	if [ -n "$dir" -a "$dir" != "/" ]; then
	    if [ -d "$dir" ]; then
		if [ -O "$dir" ]; then
		    rm -rf "$dir"
		fi
	    fi
	fi
    done

}

## @fn save_state()
## @brief Save the state of a script on a file.
## @details This function saves the state (the contents of the
## specified variables) in a file within the configuration tree. The
## full path name of the file where to save the state is determined
## with the conf_getfile() function, if it does not exist it is
## created at the deepest level of the configuration tree. The state
## can be restored in the environment with the restore_state()
## function.
## @param $1 name of the file (only name, without path) where to save the state
## @param $* the names of the environmental variables to be saved
save_state() {
    [ -z "$1" ] && return 1
 
    local state_file=`conf_getfile $1`
    [ -z "$state_file" ] && state_file=$NWPCONFDIR/$NWPCONF/$1
    shift
    for var in $*; do
	echo "$var='`eval echo '$'$var`'"
    done > $state_file
}

## @fn restore_state()
## @brief Restore the state of a script from a file.
## @details This functions restores the state which had been
## previously saved in a file within the configuration tree with the
## save_state() function. The full path name of the file where to save
## the state is determined with the conf_getfile() function.
## @param $1 name of the file (only name, without path) from where to restore the state
restore_state() {
    [ -z "$1" ] && return 1
 
    local state_file
    state_file=`conf_getfile $1`
    [ -n "$state_file" ] && . $state_file
}

# start exporting all assignments
set -a
# checks
check_dep nwpconf
check_defined NWPCONFDIR NWPCONFBINDIR NWPCONF
# create confdirlist
conf_init
# import configuration
conf_source
# init date functions
date_init
# stop exporting all assignments
set +a

