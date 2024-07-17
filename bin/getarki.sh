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
## @brief Module with functions for retrieving observations and initial/boundary conditions from arkimet archive.
## @details This module provides functions for retrieving observations
## in BUFR format and initial/boundary conditions, tipically in GRIB
## format, from the desired dataset of an [Arkimet
## archive](https://github.com/ARPA-SIMC/arkimet).
## 
## It is an optional module and it has to be sourced after the
## _nwptime.sh_ module.

## @fn getarki_obsbufr()
## @brief Retrieve observations in BUFR format from arkimet archive.
## @details This function retrieves observations in BUFR format from
## the arkimet dataset(s) specified in the configuration variables
## `$BUFR_ARKI_DS_CONV` (for which a template conversion is necessary)
## and $BUFR_ARKI_DS_NOCONV` (for which it is not necessary), for the data
## assimilation interval specified in the configuration.
## It should be called after having loaded the
## module nwptime.sh for setting up the time-related environment
## variables. The interval of data retrieved is extended by a
## configurable amount of hours (default 3), before and after the
## strict assimilation interval.
## @param $1 name of the output bufr file associated to `$BUFR_ARKI_DS_CONV`
## @param $2 name of the output bufr file associated to `$BUFR_ARKI_DS_NOCONV`
## @param $3 optional value of extra time interval in hours before and after
## assimilation, default 3
getarki_obsbufr() {

    local d t ds de dt
    dt=${3:-3}
    d=`date_sub $DATES $TIMES $dt`
    t=`time_sub $DATES $TIMES $dt`
    ds=`getarki_datetime $d $t`
    d=`date_add $DATEE $TIMEE $dt`
    t=`time_add $DATEE $TIMEE $dt`
    de=`getarki_datetime $d $t`

    [ -n "$WAITFUNCTION" ] && $WAITFUNCTION

    $SIMC_TOOLS arki-query --data -o $1 \
    "origin: BUFR; reftime:>=$ds,<=$de;" $BUFR_ARKI_DS_CONV     || true
    $SIMC_TOOLS arki-query --data -o $2 \
    "origin: BUFR; reftime:>=$ds,<=$de;" $BUFR_ARKI_DS_NOCONV   || true

}


## @fn getarki_radar_vol()
## @brief Retrieve radar volume observations in hdf5 format from arkimet archive.
## @details For each radar station specified in '$RADLIST', retrieve
## the volume closest to '$DATEE$TIMEE' (analysis time). Starting from
## '$DATEE$TIMEE', for each radar it goes back in time in '$RADAR_VOL_STEP'
## seconds steps up to '$DATES$TIMES' (excluded) until a non-empty
## file is downloaded.
getarki_radar_vol() {
    local dateobs timeobs YYYY MM DD hh mm fname dateobs_old timeobs_old

    # Time frequency (in seconds) of available data
    RADAR_VOL_STEP=600

    # Loop over radar stations
    for r in $RADLIST; do
        dateobs=$DATEE
        timeobs=${TIMEE}00
        while [ "$dateobs$timeobs" -gt "$DATES${TIMES}00" ]; do
            # Download data for specifica radar station and time
            YYYY=${dateobs:0:4}
            MM=${dateobs:4:2}
            DD=${dateobs:6:2}
            hh=${timeobs:0:2}
            mm=${timeobs:2:2}
            fname=odim_${dateobs}${timeobs}_${r}
            $SIMC_TOOLS arki-query --data -o $fname \
                "reftime:=$YYYY-$MM-$DD $hh:$mm; origin:ODIMH5,$r;" \
                $BUFR_ARKI_DS_RADARVOL

            # If file is not empty, check if elevations are in the EMVORADO defaults and
            # skip to the next radar station
            if [ -s $fname ]; then 
                $SIMC_TOOLS $WORKDIR_BASE/nwprun/ecflow/script_python3/preproc_volumi.py \
                    -f $fname -o $HDF5_QUARANTINE
                break
            fi

            # Go back in time
            dateobs_old=$dateobs
            timeobs_old=$timeobs
            dateobs=`date --date="$dateobs_old $timeobs_old -$RADAR_VOL_STEP seconds" '+%Y%m%d'`
            timeobs=`date --date="$dateobs_old $timeobs_old -$RADAR_VOL_STEP seconds" '+%H%M'`
        done
    done

    # Remove empty files
    find . -size 0 -print -delete
}


## @fn getarki_icbc()
## @brief Retrieve gridded initial and/or boundary conditions from arkimet archive.
## @details This function retrieves gridded fields, tipically in GRIB
## format, to be used as initial and/or boundary conditions, possibly
## through an interpolation process, from the arkimet dataset(s)
## specified in the configuration variable `$PARENTMODEL_ARKI_DS`, for the
## model run interval specified in the configuration. A specific model
## system module must have been loaded in order to provide the
## function inputmodel_name() for renaming the files. It should be
## called after having loaded the module nwptime.sh for setting up the
## time-related environment variables and within a time loop on input
## models, such as:
## 
##     nwpbctimeloop_init
##     while nwpbctimeloop_loop; do
##         getarki_icbc
##     done
## 
## Additional query keys for the arki-query can be specified by means
## of the variable `$MODEL_ARKI_PARAM`, terminated by `;`,
## e.g. `MODEL_ARKI_PARAM="proddef:GRIB:nn=$ENS_MEMB;"` for selecting
## a specific ensemble member as input.
getarki_icbc() {
    local h hinput timerange ana d2h t2h rest deltat_TIMES a1 a2 b1 b2

    # Redefines 'MODEL_STOP_SLICE' to retrieve the correct files for slices
    # shorter than slice frequency.
    if [ $MODEL_STOP_SLICE -lt $MODEL_FREQ_SLICE ]; then
        MODEL_STOP_SLICE=$MODEL_FREQ_SLICE
    fi

    for h in `seq $MODEL_START_SLICE $MODEL_FREQ_SLICE $MODEL_STOP_SLICE`; do
    	[ -n "$WAITFUNCTION" ] && $WAITFUNCTION $h

        # Lead time (in hours) of the file to be retrieved
    	hinput=$(($h+$MODEL_DELTABD_SLICE))

        # If 'hinput' isn't a valid lead time, i.e. it's not a multiple of
        # 'MODEL_FREQ_SLICE', the previous one available is considered. This is
        # the typical situation when you want to make assimilation cycles shorter
        # than 'PARENTMODEL_FREQ'.
        rest=`expr $hinput % $MODEL_FREQ_SLICE || true`
        if [ $rest -ne 0 ]; then
            hinput=$(($hinput-$rest))
        fi

    	timerange="timerange:Timedef,${hinput}h,254"
	    reftime=`getarki_datetime $DATES_SLICE $TIMES_SLICE`

    	ntry=2
	    ofile=`inputmodel_name $h`
    	while [ "$ntry" -gt 0 ]; do
    	    $SIMC_TOOLS arki-query --data -o $ofile \
    		"reftime:=$reftime;$timerange;$MODEL_ARKI_PARAM" $PARENTMODEL_ARKI_DS
            # if file is empty retry, otherwise exit
	    if __check_msg_num $ofile; then
		break
	    fi
    	    echo "retrying arki-query"
    	    sleep 10
    	    ntry=$(($ntry - 1))
    	done

	if ! __check_msg_num $ofile; then
    	    return 1
    	fi

        # Analysis link
        if ([ "$h" -eq "0" ] && [ $MODEL_STOP -ge $MODEL_FREQ_SLICE ]); then
            ana=`inputmodel_name a`
            [ -f "$ana" -o -h "$ana" ] || ln -s `inputmodel_name $h` $ana
        fi

        # if defined, increment progress meter
        type meter_increment 2>/dev/null && meter_increment || true
    done

    # Temporal interpolation
    if [ $MODEL_STOP -lt $MODEL_FREQ_SLICE ]; then
        # Name of files to be sorted  before interpolation
        first_file=`inputmodel_name 0`
        last_file=`inputmodel_name $h`

        # Sort gribs to ensure that they are in the same position
        $SIMC_TOOLS arki-query --data --sort=minute:timerange,level,product '' grib:$first_file > ${first_file}.sort
        $SIMC_TOOLS arki-query --data --sort=minute:timerange,level,product '' grib:$last_file  > ${last_file}.sort

        # Compute coefficients for linear combination
        deltat_TIMES=`expr $TIMES % $MODEL_FREQ_SLICE || true`
        b1=$( echo "scale=16; $deltat_TIMES/$MODEL_FREQ_SLICE" | bc )
        a1=$( echo "scale=16; 1-$b1" | bc )
        b2=$( echo "scale=16; ($deltat_TIMES+$MODEL_STOP)/$MODEL_FREQ_SLICE" | bc )
        a2=$( echo "scale=16; 1-$b2" | bc )

        # Temporal interpolation
        $SIMC_TOOLS math_grib.exe $a1 ${first_file}.sort $b1 ${last_file}.sort lfff_ini sum -check='grid'
        $SIMC_TOOLS math_grib.exe $a2 ${first_file}.sort $b2 ${last_file}.sort lfff_fin sum -check='grid'

        # Update stepRange of new files
        $SIMC_TOOLS grib_set -s stepRange=$MODEL_DELTABD_SLICE lfff_ini lfff_ini_mod
        $SIMC_TOOLS grib_set -s stepRange=$((${MODEL_DELTABD_SLICE}+${MODEL_STOP})) lfff_fin lfff_fin_mod

        # Rename files
        mv lfff_ini_mod $first_file
        newfile=`inputmodel_name $MODEL_STOP`
        mv lfff_fin_mod $newfile

        # Analysis link
        ana=`inputmodel_name a`
        ln -fs $first_file $ana

        # Remove un-necessary files
        rm lfff_ini lfff_fin $last_file
    fi
}


__check_msg_num() {

    if [ ! -s $1 ]; then # file is empty or missing
	return 1
    fi
    if [ -n "$GET_ICBC_MINCOUNT" ]; then
	nm=$($SIMC_TOOLS grib_count $1) || return 1 # file badly truncated
#	if [ -n "$nm" ]; then
	    if [ "$nm" -lt "$GET_ICBC_MINCOUNT" ]; then
		return 1 # file too short
	    fi
#	fi
    fi
}


## @fn getarki_icbc()
## @brief Retrieve gridded static data from arkimet archive.
## @details This function retrieves gridded fields, tipically in GRIB
## format, to be used as static (constant) data, possibly through an
## interpolation process, from the arkimet dataset(s) specified in the
## configuration variable `$PARENTMODEL_ARKI_DS`. It should be called
## after having loaded the module nwptime.sh for setting up the
## time-related environment variables after having initialised the
## time loop over input data, such as:
##
##     nwpbctimeloop_init
##     getarki_static
##
## Additional query keys for the arki-query can be specified by means
## of the variable `$MODEL_ARKI_PARAM`, terminated by `;`,
## e.g. `MODEL_ARKI_PARAM="proddef:GRIB:nn=$ENS_MEMB;"` for selecting
## a specific ensemble member as input.
##
## @param $1 name of the output grib file
getarki_static() {
    local h hinput timerange ana d2h t2h

    h=$MODEL_START_SLICE
    timerange="timerange:Timedef,0h,254"
    reftime=`getarki_datetime $DATES_SLICE $TIMES_SLICE`
    ntry=2
    ofile=$1
    while [ "$ntry" -gt 0 ]; do
	$SIMC_TOOLS arki-query --data -o $ofile \
	  "reftime:=$reftime;$timerange;$MODEL_ARKI_PARENT_STATIC;$MODEL_ARKI_PARAM" $PARENTMODEL_ARKI_DS
# if file is empty retry, otherwise exit
	if [ -s "$ofile" ]; then
	    break
	fi
	echo "retrying arki-query"
	sleep 10
	ntry=$(($ntry - 1))
    done
}


# The date and time as requested by reftime arki-query key
getarki_datetime() {
    if [ "${#1}" -gt 8 ]; then
	$DATECOM --date "${1:0:8} ${1:8:4}" '+%Y-%m-%d %H:00'
    else
	$DATECOM --date "$1 $2:00" '+%Y-%m-%d %H:00'
    fi
}

# start exporting all assignments
set -a
# checks
check_dep getarki nwptime
# default time to wait between checs for availability of input data
GETARKI_WAITSTART=30
# stop exporting all assignments
set +a
