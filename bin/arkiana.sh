# variables that must be set:
#
# DATES
# TIMES
# COSMO_INT2LMOUTDIR
# COSMO_STOP
# ARKI_SCAN_METHOD
# ARKI_IMPDIR
# ARKI_USE_INOTIFY
# WAITTOTAL

# COSMO_FREQANA

## @file
## @brief Module with functions for processing analysis fields in a continuous assimilation cycle.
## @details This module provides functions for merging and archiving
## analysis fields coming from a continuous model assimilation cycle
## and interpolated from a parent model.

## @fn arkiana_archive()
## @brief Retrieve analysis from the configured sources for starting a model run.
## @details This function retrieves the gridded analysis data for
## starting a model run from the configured arkimet datasets trying
## different sources for climatological, slow and fast data.
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

	# extract (~climatological) data that has to come exclusively from
	# parent model (FROM_PARENT) through int2lm and archive it
	query="$reftime;$MODEL_ARKI_FROM_PARENT"
	arki-query --data -o $parentclim "$query" $parentana.arkimet
	putarki_archive_and_wait grib $parentclim
	rm -f $parentclim
    else
	parentana=
    fi

    # extract "slow" fields from model analysis archive up to a reasonable
    # interval in the past (FROM_ANA_SLOW)
    slow_back=0
    while [ $slow_back -le "$MODEL_SLOW_PAST_H" ]; do
	slow_date=`date_sub $DATES $TIMES $slow_back`
	slow_time=`time_sub $DATES $TIMES $slow_back`
	slow_reftime="reftime:="`getarki_datetime $slow_date $slow_time`
	query="$slow_reftime; $MODEL_ARKI_TIMERANGE_ASSIM $MODEL_ARKI_FROM_ASSIM_SLOW"
	arki-query --data -o $parentslow "$query" $ARKI_DS_ASSIM
	n=`grib_count $parentslow` || n=0
	if [ $n -ge $MODEL_N_ANA_SLOW ]; then
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

    MODEL_ARKI_SURFT='level:GRIB1,111,0 or GRIB1,1;product:GRIB1,,201,197 or GRIB1,,2,85;'
    #if [ ... -a "$MODEL_REPLACE_SST" = Y ]; then
    if [ -n "$parentana" -a "$MODEL_SOIL_PARENT" = N ]; then
	# replace archived soil/surface temperature on sea with temperature
	# from parent model and lower soil boundary climatological data with
	# data from parent model
	arki-query --data -o $anasurft \
	    "$reftime;$MODEL_ARKI_TIMERANGE_ASSIM$MODEL_ARKI_SURFT" $ARKI_DS_ASSIM
	arki-query --data -o $parentsurft \
	    "$reftime;$MODEL_ARKI_SURFT" $parentana.arkimet
	arki-query --data -o $parentlsm \
	    "$reftime;$MODEL_ARKI_LSM" $parentana.arkimet
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
	    # merge in a single field and archive
	    vg6d_transform --trans-type=none --dup-mode=1 \
		$tmp1 $tmp2 $anasurft
	    archive_and_wait_grib1 $anasurft
	    rm -f $anasurft $parentsurft $parentlsm $tmp1 $tmp2
	fi # else print warning?

	if [ -n "$MODEL_ARKI_BBC" ]; then
	    # replace archived lowest soil layer with corresponding field from parent model
	    arki-query --data -o $parentbbc \
		"$reftime;$MODEL_ARKI_BBC" $parentana.arkimet
	    # set timerange indicator as for nudging?
	    #    grib_set -s timeRangeIndicator=13 $parentsurft
	    archive_and_wait_grib1 $parentbbc
	    rm -f $parentbbc
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

    if [ ! -f $parentana.arkimet ]; then
	parentana=
    fi
    arki_date=`getarki_datetime $DATES $TIMES`

    # retrieve climatological fields from parent model in archive
    timerange:GRIB1,0,0;
    arki-query --data -o $parentclim \
	"reftime:=$arki_date;$MODEL_ARKI_TIMERANGE_FCAST$MODEL_ARKI_FROM_PARENT" $ARKI_DS_FCAST
    n=`grib_count $parentclim` || n=0
    if [ $n -lt "$MODEL_N_PARENT" ]; then
	# in case of failure get them from analysis in archive
	echo "climatological fields from parent model not found in arkimet, trying to get them from model analysis"
	arki-query --data -o $parentclim \
	    "reftime:=$arki_date;$MODEL_ARKI_TIMERANGE_ASSIM$MODEL_ARKI_FROM_PARENT" $ARKI_DS_ASSIM
	MODEL_CLIM_PARENT=N
    else
	MODEL_CLIM_PARENT=Y
    fi
    # if [ "$MODEL_CLIM_PARENT" = N ]; then
    #     export COSMO_CLIM_NML=.FALSE.
    # else
    #     export COSMO_CLIM_NML=.TRUE.
    # fi

    # extract "slow" (~soil) fields from analysis in archive
    rm -f $parentslow
    [ -f "./soil_coldstart" ] || \
	arki-query --data -o $parentslow \
	"reftime:=$arki_date;$ARKI_TIMERANGE_ASSIM$MODEL_ARKI_FROM_ANA_SLOW" $ARKI_DS_ASSIM
    n=`grib_count $parentslow` || n=0

    if [ $n -lt "$MODEL_N_ANA_SLOW" ]; then
	# in case of failure get them from parent model in file
	echo "slow fields not found in archive, trying to get them from parent model"
	if [ -n "$parentana" ]; then
	    arki-query --data -o $parentslow \
		"$MODEL_ARKI_FROM_ANA_SLOW" $parentana.arkimet
	    MODEL_SLOW_ANA=N
	else
	    echo "but parent model did not provide an analysis"
	    exit 1
	fi
    else
	MODEL_SLOW_ANA=Y
    fi
    # if [ "$MODEL_SLOW" = Y ]; then
    #     export COSMO_SLOW_NML=.FALSE.
    # else
    #     export COSMO_SLOW_NML=.TRUE.
    # fi

    # extract "fast" (~atmosphere) fields from analysis in archive
    rm -f $parentfast
    [ -f "./atm_coldstart" ] || \
	arki-query --data -o $parentfast \
	"reftime:=$arki_date;$ARKI_TIMERANGE_ASSIM$MODEL_ARKI_FROM_ANA_FAST" $ARKI_DS_ASSIM
    n=`grib_count $parentfast` || n=0

    if [ $n -le 0 ]; then
	# in case of failure get them from parent model in file
	echo "fast fields not found in arkimet, trying to get them from parent model"
	if [ -n "$parentana" ]; then
	    arki-query --data -o $parentfast \
		"$MODEL_ARKI_FROM_ANA_FAST" $parentana.arkimet
	    MODEL_FAST_ANA=N
	else
	    echo "but parent model did not provide an analysis"
	    exit 1
	fi
    else
	MODEL_FAST_ANA=Y
    fi

    # glue all together
    cat $parentclim $parentslow $parentfast > $2
    rm -f $parentclim $parentslow $parentfast

}



