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
## [Arkimet archive](http://arkimet.sourceforge.net/).
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
## disappearance after archiving, all wait times are in seconds. If
## `$ARKI_USE_INOTIFY` has the value of `Y`, then the `sleep`'s between
## file checks are replaced by the `inotifywait` command, so that the
## reaction to changes in filesystem are immediate, in that case the
## aforementiond wait times are used as a timeout to inotifywait, to
## account for malfunctions of the inotify process. The value of these
## environment variables can be changed after sourcing the module.


## @fn putarki_archive_and_wait()
## @brief Archive one or more files and wait for the completion of the operation.
## @details This function archives the files passed as arguments to
## the configured dataset and waits until the files have been
## archived. It combines the putarki_archive() function for archiving
## and the putarki_wait_for_deletion() function for waiting.
## @param $1 the type of file being archived, either `grib` or `bufr`
## @param $* the files to be archived
putarki_archive_and_wait() {
    putarki_wait_for_deletion `putarki_archive $@`
}


## @fn putarki_archive()
## @brief Archive one or more files.
## @details This function archives the files passed as arguments to
## the configured dataset. The files are archived according to the
## value of `$ARKI_SCAN_METHOD`:
## 
## - `arki_importer`: assuming that a consumer process is active, the
##   files are copied to the directory `$ARKI_IMPDIR` configured in
##   the consumer, the function exits suddendly without waiting for
##   the termination of the archiving; this method is advantageous
##   because it allows concurrent processes to simultaneously send
##   data to the same dataset
## - `remote_arki_importer`: it is similar to the previous method, but
##   it assumes that the consumer process is active on a different
##   host, in this case the variable `$ARKI_IMPSSH` indicates the
##   credentials for accessing the remote import server by ssh/scp in
##   the form `user@host` and `$ARKI_IMPDIR` indicate the import
##   directory configured on the remote server; paswordless ssh access
##   to the remote server must be set up; this method allows
##   concurrency as well, but it is less performant due to the access
##   to a remote server through ssh.
## - `arki-scan`: an arki-scan is performed with configuration file
##   `$ARKI_CONF`, in this case the function exits when the archival
##   has finished; this approach does not require a consumer process,
##   but concurrent attempts to archive in the same dataset may fail
##   because of locking issues.
## 
## When using the arki_importer approach, the function prints to
## stdout the list of temporary files created in `$ARKI_IMPDIR` that
## are being imported and whose deletion indicates that the archiving
## has finished. Regardless of the archiving method, it is safe to
## remove the original file at function return since a copy (or hard
## link if possible) is made in case of asynchronous archiving.
## @param $1 the type of file being archived, either `grib` or `bufr`
## @param $* the files to be archived
putarki_archive() {

    local file
    local tf
    tf=$1
    shift
    for file in $@; do
    case "$ARKI_SCAN_METHOD" in
        arki_importer)
# Best method, it avoids concurrency problems but it requires a
# consumer process to be active, typically running under the same user
# id (see script arki_importer.sh); warning, it is an asynchronous
# method, in order to be sure that the file has been completely
# imported, archive_and_wait_grib1 function has to be used instead
            cd $ARKI_IMPDIR
            dest=`mktemp .XXXXXXXX.$tf`
            cd - 1>/dev/null
            ddest=${dest#.}
# try with a hard link, avoiding copy
            cp -f -l $file $ARKI_IMPDIR/$dest || cp -f $file $ARKI_IMPDIR/$dest
            mv -f $ARKI_IMPDIR/$dest $ARKI_IMPDIR/$ddest
# print file name on stdout, to be used by archive_and_wait_grib1
            echo $ARKI_IMPDIR/$ddest;;
        remote_arki_importer)
	    scp $file $ARKI_IMPSSH:$ARKI_IMPDIR/.$file.$$.$tf
	    ssh $ARKI_IMPSSH mv -f $ARKI_IMPDIR/.$file.$$.$tf $ARKI_IMPDIR/$file.$$.$tf
            echo $ARKI_IMPDIR/$file.$$.$tf;;
        arki-scan)
# do a simple, local, file-based arki-scan, the user must deal with
# concurrency problems; synchronous method
            arki-scan --dispatch=$ARKI_CONF $tf:$file > /dev/null;;
    esac
    done
}


## @fn putarki_wait_for_deletion()
## @brief Wait until the requested files have been deleted.
## @details This function waits until the files passed as arguments
## (which should reside in `$ARKI_IMPDIR`) have been deleted, meaning
## that their archiving has completed. If configured by the
## environment assignment `ARKI_USE_INOTIFY=Y`, the function uses
## the command `inotifywait` to speedup the detection of the deletion
## process, otherwise a series of check and sleep is performed. All
## the operations, regardless of the use of inotify, have a timeout of
## `$PUTARKI_WAITDEL` seconds. It is a function specific to the
## putarki_archive() function, it should not be used as a generic file
## deletion checking function.
## @param $* the files to be deleted
putarki_wait_for_deletion() {

    local waitlist=("$@")
    while true; do
        for i in ${!waitlist[*]}; do
	    case "$ARKI_SCAN_METHOD" in
		arki_importer)
		    if [ ! -f ${waitlist[$i]} ]; then
			unset waitlist[$i]
		    fi
		    ;;
		remote_arki_importer)
		    if ! ssh $ARKI_IMPSSH test -f ${waitlist[$i]}; then
			unset waitlist[$i]
		    fi
		    ;;
	    esac
        done
# check if there are still files to wait for
        if [ ${#waitlist[*]} -eq 0 ]; then
            return
        fi
# make a break
        if [ "$ARKI_SCAN_METHOD" = arki_importer -a "$ARKI_USE_INOTIFY" = Y ]; then
            inotifywait --timeout $PUTARKI_WAITDEL --event delete $ARKI_IMPDIR >/dev/null 2>&1 || true
        else
            sleep $PUTARKI_WAITDEL
        fi
    done
        
}


## @fn putarki_model_output()
## @brief Archive the output of a model run while it is being produced.
## @details This function waits for the appearing of model ouput files
## and archives them as soon as possible in the configured Arkimet
## dataset with the putarki_archive() function. It relies on the
## creation, in the current directory, of ready-files with name
## matching `$READYFILE_PATTERN` and on the existance of a
## model-specific function model_readyfiletoname() taking as argument
## the name of a specific ready-file and printing to stdout the name
## of all the output files related to that ready-file. If the
## `$PUTARKI_WAITTOTAL` variable is defined, the function will exit after that
## number of seconds. even if the work is not finished. For an example
## of model-specific setup, see the documentation of the
## _cosmo_module.sh_ module.
## @param $1 the number of ready files to wait for before exiting
## @param $2 optional, if equal to `-w` tells the function to wait for the archiving to complete before exiting
putarki_model_output() {

# initialisations
    local sleepint=$PUTARKI_WAITSTART
    local initialtime=`date -u +%s`
    local workdir=$PWD
    local nrfiles=$1
    local wait=
    [ "$2" = "-w" ] && wait=Y
    local rfile
    declare -a waitlist
    declare -A statuslist
    statuslist=()

    while true; do
# this is done here in case the directory is removed and recreated
	cd $workdir
	waitlist=()
	found=
# loop on ready-files
	shopt -s nullglob
	for rfile in $READYFILE_PATTERN; do
	    if [ -z "${statuslist[$rfile]}" ]; then # it is a new file
		echo $rfile
# process all grib files related to $rfile
		for gfile in `model_readyfiletoname $rfile`; do
		    echo $gfile
		    waitlist[${#waitlist[*]}]=`putarki_archive grib $gfile`
		done
# update status for $rfile
		statuslist[$rfile]="DONE"
# if defined, increment progress meter
		type meter_increment 2>/dev/null && meter_increment || true
		found=Y
	    fi
	done
	shopt -u nullglob

	if [ -n "$found" ]; then # something new has been found
	    sleepint=$PUTARKI_WAITSTART
	    if [ ${#waitlist[*]} -gt 0 -a -n "$wait" ]; then
		putarki_wait_for_deletion ${waitlist[*]}
	    fi
	else # nothing new has been found
# end of task condition
#	    if [ "${statuslist[$1]}" = "DONE" ]; then
	    if [ ${#statuslist[*]} -eq $nrfiles ]; then 
		return
	    fi
# end of time
# disable if running under a scheduler?
	    if [ -n "$PUTARKI_WAITTOTAL" ]; then
		if [ $((`date -u +%s` - $initialtime)) -gt "$PUTARKI_WAITTOTAL" ]; then
		    echo "Timeout reached, exiting"
		    exit 1
		fi
	    fi
# wait for some event
	    if [ "$ARKI_USE_INOTIFY" = Y ]; then
		inotifywait --timeout $PUTARKI_WAITSTART --event close --exclude '^\.\/l.*' ./ || true
	    else
		sleep $sleepint
		if [ "$sleepint" -lt "$PUTARKI_WAITMAX" ]; then
		    sleepint=$(($sleepint * 2))
		fi
	    fi
	fi
    done

}


# start exporting all assignments
set -a
check_dep putarki
check_defined ARKI_SCAN_METHOD
# default time to wait before starting processing output files during model run (s)
PUTARKI_WAITSTART=30
# default maximum time to wait between checks when no events happen (s)
PUTARKI_WAITMAX=300
# default time to wait between checks for files to be archived (s)
PUTARKI_WAITDEL=5
# stop exporting all assignments
set +a
