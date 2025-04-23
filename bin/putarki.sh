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
## @brief Modules with functions for archiving the model result to a configured Arkimet dataset.
## @details This module provides functions for archiving files,
## tipically model output in GRIB format, into a desired dataset of an
## [Arkimet archive](https://github.com/ARPA-SIMC/arkimet/).
## 
## It is an optional module and it has to be sourced after the
## _nwptime.sh_ module.
## 
## When sourcing the module, the following default assignments are made:
## 
##     PUTARKI_WAITSTART=30
##     PUTARKI_WAITMAX=300
##     PUTARKI_WAITDEL=5
## 
## `$PUTARKI_WAITSTART` is the initial wait time between checks for
## the appearance of new output files, `$PUTARKI_WAITMAX` is the
## maximum wait time between checks when no events happen,
## `$PUTARKI_WAITDEL` is the wait time between checks of file
## disappearance after archiving, all wait times are in seconds.


## @fn putarki_archive()
## @brief Archive one or more files.

## @details This function dispatches the files passed as arguments to
## the configured destinations. The files are dispatched to three
## possible destinations: according to the _configured import__
## protocol:
##  * temporary directory defined by the variable `$ARKI_IMPDIR` and
##    by the optional variable `$IMPORT_THREAD` for successive import
##    into a local arkimet by an instance of
##    `threaded_multi_importer.sh` process
##  * temporary directory(ies) defined by the `$ARKI_SYNCDIR` array
##    for successive syncing to remote destination(s) by instance(s)
##    of `threaded_multi_importer.sh` process
##  * long-term storage directory defined by the `$ARKI_DLDIR`
##    variable for exposing files in a download area
## @param $1 the type of file being archived, either `grib` or `bufr`
## @param $* the files to be archived
putarki_archive() {

    local file
    local tf
    tf=$1
    shift
    for file in $@; do
	$SIMC_TOOLS arki-scan --dispatch=$ARKI_CONF $tf:$file > /dev/null
    done
}


putarki_configured_model_output() {

# initialisations
    declare -Ag statuslist
    statuslist=()
    NWPWAITWAIT=$PUTARKI_WAITSTART
    NWPWAITSOLAR=
    nwpwait_setup
    putarki_configured_setup $MODEL_SIGNAL "reftime=$DATE$TIME" "signal=$MODEL_SIGNAL" "signal_method=$CONFIGURED_SIGNAL_METHOD"

    while true; do
	putarki_configured_model_output_get_one	$1 $PWD $PWD
        if [ "$retval" = 0 ]; then
	    break
	fi
	nwpwait_wait
    done
    putarki_configured_end $MODEL_SIGNAL
    return

}


putarki_configured_model_output_get_one() {
    trap "retval=1; return 0" ERR
    # propagate the error trap to called functions
    set -o errtrace
    retval=0 # default return status: finished
    local rfile found
    local nrfiles=$1

    while true; do
# this is done here in case the directory is removed and recreated
	if [ -n " $2" ]; then # remove if
	    if [ ! -d "$2" ]; then
		false # the run has probably been interrupted, return and wait for a restart
	    fi
            cd $2
	fi
	found=
# loop on ready-files
	shopt -s nullglob
# this trick is required if pattern contains {*,?} because brace({,})
# expansion is done before variable expansion
	matchlist=`eval echo "$READYFILE_PATTERN"`
	for rfile in $matchlist; do
	    if [ -z "${statuslist[$rfile]}" ]; then # it is a new file
                log "found ready-file $rfile"
# process all grib files related to $rfile
		for gfile in `model_readyfiletoname $rfile`; do
                    log "processing $gfile"
		    if [ -n "$POSTPROC_FUNC" ]; then
			$POSTPROC_FUNC $gfile $MODEL_SIGNAL &
		    else
			putarki_configured_archive $MODEL_SIGNAL $gfile grib
			# create and archive postprocessed data if required
			for ppc in ${POSTPROC_LIST[*]}; do
			    ext=${ppc##*_}
			    $ppc $gfile $3/${gfile}_${ext}
			    [ "$retval" = "0" ] || false
			    [ -s "$3/${gfile}_${ext}" ] && putarki_configured_archive $MODEL_SIGNAL $3/${gfile}_${ext} $POSTPROC_FORMAT
			    rm -f $3/${gfile}_${ext}
			done
		    fi
		done
		wait # for the $POSTPROC_FUNC case
# update status for $rfile
		statuslist[$rfile]="DONE"
# if defined, increment progress meter
		type meter_increment 2>/dev/null && meter_increment || true
		found=Y
	    fi
	done
	shopt -u nullglob

	if [ -z "$found" ]; then # nothing new has been found
            if [ ${#statuslist[*]} -eq $nrfiles ]; then
		retval=0 # end of run, consider not to set retval=0, in case some function has failed silently
		return
            else
		false
            fi
	fi
    done

}


## @fn putarki_configured_setup()
## @brief Start a session of configured archiving.
## @details This function starts a session of configured archiving
## creating the archiving directory (local or remote) below
## `$ARKI_IMPDIR/configured` and `$ARKI_DLDIR` and populating it with
## the configuration file for the importer `start.sh`.
## @param $1 the (unique) name of the directory to be created relative to $ARKI_IMPDIR/configured
## @param $* the parameters to be set in the configuration file in form `key=val`
putarki_configured_setup() {

    local dir=$1:$DATE$TIME:$ENS_MEMB:
    local putdir
    shift
    if [ -n "$ARKI_IMPDIR" ]; then
	__putarki_configured_setup $ARKI_IMPDIR/configured${IMPORT_THREAD:+.$IMPORT_THREAD}/$dir.$$ $@
    fi
    if [ -n "$ARKI_SYNCDIR" ]; then
	for putdir in "${ARKI_SYNCDIR[@]}"; do
	    __putarki_configured_setup $putdir/$dir.$$ $@
	done
    fi
    if [ -n "$ARKI_DLDIR" ]; then
	__putarki_configured_setup $ARKI_DLDIR/configured/$dir $@
    fi

}

__putarki_configured_setup() {

    local dir=$1
    shift
    if [ -d "$dir" ]; then
	safe_rm_rf $dir
    fi
    mkdir -p $dir || return 1
    rm -f $dir/.start.sh $dir/start.sh
    for var in "$@"; do
	echo $var >> $dir/.start.sh
    done
    mv -f $dir/.start.sh $dir/start.sh
}


## @fn putarki_configured_archive()
## @brief Archives one file in the set of configured destinations.
## @details This function dispatches the files passed as arguments to
## the configured destinations. The files are dispatched to three
## possible destinations: according to the __configured import__
## protocol:
##  * temporary directory defined by the variable `$ARKI_IMPDIR` and
##    by the optional variable `$IMPORT_THREAD` for successive import
##    into a local arkimet archive by an instance of
##    `threaded_multi_importer.sh` process
##  * temporary directory(ies) defined by the `$ARKI_SYNCDIR` array
##    for successive syncing to remote destination(s) by instance(s)
##    of `threaded_multi_importer.sh` process
##  * long-term storage directory defined by the `$ARKI_DLDIR`
##    variable for exposing files in a download area
##
## The destination directories must have been created with the
## `putarki_configured_setup function`.
## @param $1 the (unique) name of the upload directory as specified in putarki_configured_setup
## @param $2 data file to be uploaded for archiving
## @param $3 (optional) suffix to be appended to the file to indicate its format, without dot character (e.g. \a grib)
putarki_configured_archive() {

    local dir=$1:$DATE$TIME:$ENS_MEMB:
    local putdir
    if [ -n "$ARKI_IMPDIR" ]; then
	__putarki_configured_archive $ARKI_IMPDIR/configured${IMPORT_THREAD:+.$IMPORT_THREAD}/$dir.$$ $2 $3
    fi
    if [ -n "$ARKI_SYNCDIR" ]; then
	for putdir in "${ARKI_SYNCDIR[@]}"; do
	    __putarki_configured_archive $putdir/$dir.$$ $2 $3
	done
    fi
    if [ -n "$ARKI_DLDIR" ]; then
	__putarki_configured_archive $ARKI_DLDIR/configured/$dir $2 $3
    fi

}

__putarki_configured_archive() {

    local dir=$1
    local file=${2##*/}
    local ext
    if [ -n "$3" ]; then
	ext=".$3"
    else
	ext=""
    fi
    [ -f "$dir/start.sh" ] || return 1
    cp -f -l $2 $dir/$file$ext || rsync -p $2 $dir/$file$ext

}


## @fn putarki_configured_end()
## @brief Closes a configured archiving session.
## @details This function closes a configured archiving session by
## creating a conventional `end.sh` file. After this operation no more
## files can be added for archiving. The calling process does not need
## to perform any other operation, the archiving daemon will take care
## of archiving the files and signalling the completion if requested.
## @param $1 the (unique) name of the upload directory as specified in putarki_configured_setup
putarki_configured_end() {

    local dir=$1:$DATE$TIME:$ENS_MEMB:
    local putdir
    if [ -n "$ARKI_IMPDIR" ]; then
	__putarki_configured_end $ARKI_IMPDIR/configured${IMPORT_THREAD:+.$IMPORT_THREAD}/$dir.$$
    fi
    if [ -n "$ARKI_SYNCDIR" ]; then
	for putdir in "${ARKI_SYNCDIR[@]}"; do
	    __putarki_configured_end $putdir/$dir.$$
	done
    fi
    if [ -n "$ARKI_DLDIR" ]; then
	__putarki_configured_end $ARKI_DLDIR/configured/$dir
    fi

}

__putarki_configured_end() {

    local dir=$1
    [ -f "$dir/start.sh" ] || return 1
    touch $dir/end.sh

}


# @fn putarki_configured_dailycleanup
# @brief Clean up the configured import directory.
# @details This function cleans up the content of the configured
# import directory tree from subdirectories older than the requested
# number of days. It scans the directories indicated by \a
# $ARKI_IMPDIR and \a $ARKI_DLDIR variables if set.
# @param $1 number of days to keep in the import directory tree with respect to current date
putarki_configured_dailycleanup() {

    dt=`date_now`
    dt=`datetime_sub ${dt}00 $(($1 * 24))`00
    if [ -n "$ARKI_IMPDIR" ]; then
	for dir in $ARKI_IMPDIR/configured.*/ $ARKI_IMPDIR/sync.*/; do # $IMPORT_THREAD?
	    if [ -d "$dir" ]; then
		__putarki_configured_dailycleanup $dir $dt
	    fi
	done
    fi
    if [ -n "$ARKI_DLDIR" ]; then
	__putarki_configured_dailycleanup $ARKI_DLDIR/configured/ $dt
    fi

}

__putarki_configured_dailycleanup() {

    pushd $1 > /dev/null || return
    local dt=$2
    for dts in *; do
	if [ -d "$dts" ]; then
	    dtnum=`echo $dts | cut -d : -f 2`
            local dtl=${#dtnum}
	    # if dtnum/dt non numeric, condition is not met
	    if [ "$dtnum" -lt "${dt:0:$dtl}" ] 2>/dev/null; then
		safe_rm_rf $dts
	    fi
	fi
    done
    popd > /dev/null

}


# start exporting all assignments
set -a
check_dep putarki
# default time to wait before starting processing output files during model run (s)
PUTARKI_WAITSTART=30
# default maximum time to wait between checks when no events happen (s)
PUTARKI_WAITMAX=300
# default time to wait between checks for files to be archived (s)
PUTARKI_WAITDEL=5
# stop exporting all assignments
set +a
