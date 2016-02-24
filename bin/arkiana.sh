## @file
## @brief Module with functions for processing analysis fields in a continuous assimilation cycle.
## @details This module provides functions for merging and archiving
## analysis fields coming from a continuous model assimilation cycle
## and interpolated from a parent model. It requores definition of the
## following environmental variables (explanation to be added):
##
## - `$MODEL_ARKI_FROM_PARENT`
##
## - `$MODEL_N_PARENT`
##
## - `$MODEL_SOIL_PARENT`
##
## - `$MODEL_ARKI_FROM_ASSIM_SLOW`
##
## - `$MODEL_N_ASSIM_SLOW`
##
## - `$MODEL_ARKI_FROM_ASSIM_FAST`
##
## - `$MODEL_N_ASSIM_FAST`
##
## - `$MODEL_ARKI_TIMERANGE_ASSIM`
##
## - `$MODEL_ARKI_TIMERANGE_FCAST`
##
## - `$ARKI_DS_ASSIM`
##
## - `$ARKI_DS_FCAST`
##
## - `$ARKI_DS_INTER`
##
## - `$MODEL_ASSIM_GP`
##
## - `$MODEL_FCAST_GP`
##
## - `$MODEL_INTER_GP`
##
## - `$MODEL_ARKI_SURFT`
##
## - `$MODEL_ARKI_LSM`
##
## - `$MODEL_ARKI_BBC`

## @fn arkiana_archive()
## @brief Archive interpolated analysis and merge surface fields with the ones available from assimilation.
## @details This function selects, from an interpolated analysis file,
## only those fields that should come from parent model as selected by
## the variable `$MODEL_ARKI_FROM_PARENT` and archives them for being
## used in the successive model run. It also merges parent model sea
## surface temperature with soil surface temperature from analysis, if
## necessary and available, and archives it. It also checks for
## availability of so-called "slow" fields in the analysis, as
## selected by the variable `$MODEL_ARKI_FROM_ASSIM_SLOW`, and if
## found within an interval of `$MODEL_SLOW_PAST_H` hours in the past,
## it refreshes the date in grib fields and rearchives them for
## successive use.
## @param $1 the name of the file containing parent model interpolated analysis
arkiana_archive() {
    local parentana=$1
    [ -n "$parentana" ] || return 1 # improve
    local query
    local parentclim=clim.grib
    local parentslow=slow.grib
    local parentsurft=surft.grib
    local anasurft=asurft.grib
    local parentlsm=lsm.grib
    local parentbbc=bbc.grib
    local tmp1=tmp1.grib
    local tmp2=tmp2.grib

    # at this point interpolated analysis must have already been renamed
    # to *_parent
    reftime="reftime:="`getarki_datetime $DATES $TIMES`
    if [ -f $parentana ]; then
	# index the complete interpolated analysis
	arki-scan grib1:$parentana > $parentana.arkimet

	# retrieve (~climatological) data that has to come exclusively from
	# parent model (FROM_PARENT) and archive it
	arki-query --data -o $parentclim "$reftime;$MODEL_ARKI_FROM_PARENT;" \
	    $parentana.arkimet
	putarki_archive_and_wait grib $parentclim
	rm -f $parentclim
    else
	parentana=
    fi

    # retrieve "slow" fields from model analysis archive up to a reasonable
    # interval in the past (FROM_ASSIM_SLOW)
    slow_back=0
    while [ $slow_back -le "$MODEL_SLOW_PAST_H" ]; do
	slow_date=`date_sub $DATES $TIMES $slow_back`
	slow_time=`time_sub $DATES $TIMES $slow_back`
	slow_reftime="reftime:="`getarki_datetime $slow_date $slow_time`
	arki-query --data -o $parentslow \
	    "$slow_reftime;$MODEL_ARKI_TIMERANGE_ASSIM;$MODEL_ARKI_FROM_ASSIM_SLOW;" \
	    $ARKI_DS_ASSIM
	n=`grib_count $parentslow` || n=0
	if [ $n -ge $MODEL_N_ASSIM_SLOW ]; then
	    # found something
            if [ $slow_back -gt 0 ]; then
		# move forward the date and rearchive
		grib_set -s dataDate=$DATES,dataTime=${TIMES}00 \
		    $parentslow $parentslow.update
		mv $parentslow.update $parentslow
		putarki_archive_and_wait grib $parentslow
            fi
	    break
	fi
	slow_back=$(($slow_back + 24)) # fixed to 24 for daily cycle
    done
    rm -f $parentslow

    #if [ ... -a "$MODEL_REPLACE_SST" = Y ]; then
    if [ -n "$parentana" -a "$MODEL_SOIL_PARENT" = N ]; then
	# replace archived soil/surface temperature on sea with temperature
	# from parent model and lower soil boundary climatological data with
	# data from parent model
	arki-query --data -o $anasurft \
	    "$reftime;$MODEL_ARKI_TIMERANGE_ASSIM;$MODEL_ARKI_SURFT;" $ARKI_DS_ASSIM
	arki-query --data -o $parentsurft \
	    "$reftime;$MODEL_ARKI_SURFT;" $parentana.arkimet
	arki-query --data -o $parentlsm \
	    "$reftime;$MODEL_ARKI_LSM;" $parentana.arkimet
	n=`grib_count $anasurft` || n=0
	n1=`grib_count $parentsurft` || n1=0
	n2=`grib_count $parentlsm` || n2=0
	if [ $n -ge 1 -a $n1 -ge 1 -a $n2 -ge 1 ]; then
	    # use vg6d_transform to split sea/land temperature
	    # extract sea
	    vg6d_transform --trans-type=metamorphosis --sub-type=maskvalid \
		--maskbounds=-0.5,0.5 --coord-file=$parentlsm --coord-format=grib_api \
		$parentsurft $tmp1
	    # extract land
	    vg6d_transform --trans-type=metamorphosis --sub-type=maskvalid \
		--maskbounds=0.5,1.5 --coord-file=$parentlsm --coord-format=grib_api \
		$anasurft $tmp2
	    # merge in a single field
	    vg6d_transform --trans-type=none --dup-mode=1 $tmp1 $tmp2 $anasurft
	    # set generating process as if they came from assimilation
	    grib_set -s generatingProcessIdentifier=$MODEL_ASSIM_GP \
		$anasurft $anasurft.gp
	    putarki_archive_and_wait grib $anasurft.gp
	fi # else print warning?
	rm -f $anasurft $parentsurft $parentlsm $tmp1 $tmp2 $anasurft.gp

	if [ -n "$MODEL_ARKI_BBC" ]; then
	    # replace archived lowest soil layer with corresponding field from parent model
	    arki-query --data -o $parentbbc "$reftime;$MODEL_ARKI_BBC;" \
		$parentana.arkimet
	    # set timerange indicator as for nudging?
	    #    grib_set -s timeRangeIndicator=13 $parentbbc
	    # set generating process as if they came from assimilation
	    grib_set -s generatingProcessIdentifier=$MODEL_ASSIM_GP \
		$parentbbc $parentbbc.gp
	    putarki_archive_and_wait grib $parentbbc.gp
	    rm -f $parentbbc $parentbbc.gp
	fi

    fi

}


## @fn arkiana_retrieve()
## @brief Retrieve analysis from the configured sources for starting a model run.
## @details This function retrieves the gridded analysis data for
## starting a model run from the configured arkimet datasets trying
## different sources for climatological, slow and fast data.
## @param $1 the name of the file containing parent model interpolated analysis, if empty it is not used
## @param $2 the name of the output analysis file to be created
arkiana_retrieve() {

    local parentana=$1
    local parentclim=clim.grib
    local parentslow=slow.grib
    local parentfast=fast.grib

    if [ ! -f $parentana.arkimet ]; then
	parentana=
    fi
    arki_date=`getarki_datetime $DATES $TIMES`

    # retrieve climatological (and possibly other) fields from parent
    # model in archive
    arki-query --data -o $parentclim \
	"reftime:=$arki_date;$MODEL_ARKI_TIMERANGE_FCAST;$MODEL_ARKI_FROM_PARENT;" \
	$ARKI_DS_INTER
    n=`grib_count $parentclim` || n=0
    if [ $n -lt "$MODEL_N_PARENT" ]; then
	# in case of failure get them from analysis in archive
	echo "climatological fields not found in parent model archive, trying to get them from analysis archive"
	arki-query --data -o $parentclim \
	    "reftime:=$arki_date;$MODEL_ARKI_TIMERANGE_ASSIM;$MODEL_ARKI_FROM_PARENT;" \
	    $ARKI_DS_ASSIM
	MODEL_CLIM_PARENT=N
	n=`grib_count $parentclim` || n=0
	if [ $n -lt "$MODEL_N_PARENT" ]; then
	    echo "climatological fields not found neither in analysis archive"
	    exit 1
	fi
    else
	MODEL_CLIM_PARENT=Y
    fi

    # retriev "slow" (~soil) fields from analysis in archive
    rm -f $parentslow
    [ -f "./soil_coldstart" ] || \
	arki-query --data -o $parentslow \
	"reftime:=$arki_date;$MODEL_ARKI_TIMERANGE_ASSIM;$MODEL_ARKI_FROM_ASSIM_SLOW;" \
	$ARKI_DS_ASSIM
    n=`grib_count $parentslow` || n=0
    if [ $n -lt "$MODEL_N_ASSIM_SLOW" ]; then
	MODEL_SLOW_ASSIM=N
	# in case of failure get them from parent model in file
	echo "slow fields not found in analysis archive, trying to get them from parent model output"
	if [ -n "$parentana" ]; then
	    arki-query --data -o $parentslow \
		"$MODEL_ARKI_FROM_ASSIM_SLOW;" $parentana.arkimet
	    n=`grib_count $parentslow` || n=0
	    if [ $n -lt "$MODEL_N_ASSIM_SLOW" ]; then
		echo "slow fields not found neither in parent model output"
	    fi
	else
	    echo "parent model output not available"
	    exit 1
	fi
    else
	MODEL_SLOW_ASSIM=Y
    fi
    # if [ "$MODEL_SLOW" = Y ]; then
    #     export COSMO_SLOW_NML=.FALSE.
    # else
    #     export COSMO_SLOW_NML=.TRUE.
    # fi

    # retrieve "fast" (~atmosphere) fields from analysis in archive
    rm -f $parentfast
    [ -f "./atm_coldstart" ] || \
	arki-query --data -o $parentfast \
	"reftime:=$arki_date;$MODEL_ARKI_TIMERANGE_ASSIM;$MODEL_ARKI_FROM_ASSIM_FAST" \
	$ARKI_DS_ASSIM
    n=`grib_count $parentfast` || n=0
    if [ $n -le "$MODEL_N_ASSIM_FAST" ]; then
	MODEL_FAST_ASSIM=N
	# in case of failure get them from parent model in file
	echo "fast fields not found in analysis archive, trying to get them from parent model output"
	if [ -n "$parentana" ]; then
	    arki-query --data -o $parentfast \
		"$MODEL_ARKI_FROM_ASSIM_FAST;" $parentana.arkimet
	    n=`grib_count $parentfast` || n=0
	    if [ $n -lt "$MODEL_N_ASSIM_FAST" ]; then
		echo "fast fields not found neither in parent model output"
	    fi
	else
	    echo "parent model output not available"
	    exit 1
	fi
    else
	MODEL_FAST_ASSIM=Y
    fi

    # glue all together
    cat $parentclim $parentslow $parentfast > $2
    rm -f $parentclim $parentslow $parentfast

}

# start exporting all assignments
set -a
# checks
check_dep arkiana getarki putarki
check_defined MODEL_SLOW_PAST_H
# stop exporting all assignments
set +a
