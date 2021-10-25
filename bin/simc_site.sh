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


# Check for an event in the SIMC logging system.
# Returns true (0) if the event is present in the logging system.
# @param $1 name of event
# @param $1 nominal date and time of event (`+%Y%m%d%H%M`, UTC),
# @param $1 specific message to wait for within an event (if empty end of event will match)
simc_check_logevent() {

    res=`curl -s --fail \
            "http://log.metarpa/simclog2/api/v2/entity/${1}/check_import/${2}?message_pattern=${3}" || true`
    if [ "$res" = '{"found":true}' ]; then
	echo 1
    else
	echo 0
    fi
}

# Wait for the logsim event concerning availability of observations in
# bufr format in the arkimet archive.
# @param $1 name of the logsim event to wait for (typically "CNMCA;BUFRLM" or "CNMCA;BUFRUC")
simc_wait_obs() {

    local d t ds

# date = yyyy-mm-dd hh (+03 per COSMO-I7/+00 per RUC)
# name = CNMCA;BUFRLM/CNMCA;BUFRUC
    if [ "$1" = "CNMCA;BUFRLM" ]; then
	d=`date_add $DATE $TIME 3`
	t=`time_add $DATE $TIME 3`
	ds=$d$t
    else
	ds=$DATE$TIME
    fi

    simc_wait_logevent "" "${ds}00" $1

}



# Wait for an event in the SIMC logging system.
# @param $1 specific message to wait for within an event (if empty any message will match)
# @param $1 nominal date and time of event (`+%Y%m%d%H%M`, UTC), 
# @param $3 name of event
simc_wait_logevent() {

    while true; do
        res=`curl -s --fail \
            "http://log.metarpa/simclog2/api/v2/entity/${3}/check_import/${2}?message_pattern=${1}" || true`
        if [ "$res" = '{"found":true}' ]; then
            return 0
        fi

	sleep $GETARKI_WAITSTART
    done
}


# Signal an event to the SIMC logging system.
# Important environment variables:
# LOGSIM_PROCESS
# DATE
# TIME
# @param $* list of file to signal, if not provided only a single event without associated file is signalled
simc_send_logevent() {
    if [ -n "$LOGSIM_PROCESS" ]; then
	for file in "$@"; do
	    if [ -z "$file" ]; then
		MESSAGE="`date -u +%Y%m%d%H%M%S`;${DATE}${TIME}00;SIMC;$LOGSIM_PROCESS"
	    else
		MESSAGE="`date -u +%Y%m%d%H%M%S`;${DATE}${TIME}00;SIMC;$LOGSIM_PROCESS;${file##*/}"
	    fi
	    logger -p local3.info -t import_models "$MESSAGE"
	done
    fi
}

# Important environment variables:
# RADAR_MOSAICODIR=$HOME/prelhn/Composito
# RADAR_MOSAICOCONF=$RADAR_MOSAICODIR/configurazioni/DPC.CONF
# RADAR_LHNDIR=$HOME/prelhn/bufr2grib-RUC
# stdout and error handling must be improved
# 
# @param $1 previous end date and time (`+%Y%m%d%H%M`, UTC)
# @param $2 end date and time (`+%Y%m%d%H%M`, UTC)
simc_create_radar_grib() {
    local model_template fromdate todate succdate ncmosaico gribmosaico waitfor

    succdate=
    waitfor=
# get grib template on the model grid
    model_template=`conf_getfile model_radar_template.grib`
    if [ -z "$model_template" ]; then
	echo "Error: grib template model_radar_template.grib for radar precipitation gridding not found in configuration directories" >&2
	return 1
    fi

    fromdate=$1
    todate=$2
# increment date
    fromdate=$(date -u --date "${fromdate:0:8} ${fromdate:8:4} $RADAR_DT minutes" "+%Y%m%d%H%M")
# loop over radar-precipitation-rate time levels
    while [ "$fromdate" -le "$todate" ]; do
# compute filenames for current date
	ncmosaico=SURF_RATE_$fromdate.nc
	gribmosaico=radar_SRI_$fromdate.grib1

# create mosaico in netcdf format ($ncmosaico) from SIMC archives
#	$RADAR_MOSAICODIR/Mosaico.sh $RADAR_MOSAICODIR/mosaico.config -dn $fromdate -C $RADAR_MOSAICOCONF>/dev/null 2>&1
	$RADAR_MOSAICODIR/creo_nc_lhn.bash $fromdate

	if [ -f $ncmosaico ]; then 
	    rm -f $gribmosaico
# create precipitation grib file ($gribmosaico) from netcdf
# ($ncmosaico) for current date and archive
	    $RADAR_LHNDIR/netcdf2grib1_SIMC $ncmosaico $model_template>/dev/null 2>&1
	    if [ -f $gribmosaico ]; then
		if [ -n "$RADAR_LHN_GP" ]; then
		    grib_set -s generatingProcessIdentifier=$RADAR_LHN_GP \
			$gribmosaico $gribmosaico.gp>/dev/null
		    mv -f $gribmosaico.gp $gribmosaico
		fi
# archive and remember for final waiting
		if [ -n "$ARKI_SCAN_METHOD" ]; then
		    waitfor="$waitfor `putarki_archive grib $gribmosaico`"
		    rm -f $gribmosaico
		fi
	    fi
	    rm -f $ncmosaico
# store successful date
	    succdate=$fromdate
	fi

# increment date
	fromdate=$(date -u --date "${fromdate:0:8} ${fromdate:8:4} $RADAR_DT minutes" "+%Y%m%d%H%M")
    done
# wait once for all to avoid useless pauses
    [ -n "$waitfor" ] && putarki_wait_for_deletion $waitfor || true
# output last processed date
    echo $succdate
}


# Important environment variables:
# RADAR_MOSAICODIR=$HOME/prelhn/Composito
# RADAR_MOSAICOCONF=$RADAR_MOSAICODIR/configurazioni/DPC.CONF
# stdout and error handling must be improved
# 
# @param $1 previous end date and time (`+%Y%m%d%H%M`, UTC)
# @param $2 end date and time (`+%Y%m%d%H%M`, UTC)
simc_create_radar_nc() {
    local fromdate todate succdate ncmosaico

    succdate=
    fromdate=$1
    todate=$2
# increment date
    fromdate=$(date -u --date "${fromdate:0:8} ${fromdate:8:4} $RADAR_DT minutes" "+%Y%m%d%H%M")
# loop over radar-precipitation-rate time levels
    while [ "$fromdate" -le "$todate" ]; do
# compute filenames for current date
	ncmosaico=SURF_RATE_$fromdate.nc
	rm -f $ncmosaico
	
# create mosaico in netcdf format ($ncmosaico) from SIMC archives
#	$RADAR_MOSAICODIR/Mosaico.sh $RADAR_MOSAICODIR/mosaico.config -dn $fromdate -C $RADAR_MOSAICOCONF>/dev/null 2>&1
	$RADAR_MOSAICODIR/creo_nc_lhn.bash $fromdate >/dev/null 2>&1

	if [ -f $ncmosaico ]; then 
# store successful date
            succdate=$fromdate
	fi

# increment date
	fromdate=$(date -u --date "${fromdate:0:8} ${fromdate:8:4} $RADAR_DT minutes" "+%Y%m%d%H%M")
    done
# output last processed date
    echo $succdate
}

# Important environment variables:
# RADAR_LHNDIR=$HOME/prelhn/bufr2grib-RUC
# stdout and error handling must be improved
simc_convert_radar_grib() {
    local model_template gribmosaico waitfor

    waitfor=
# get grib template on the model grid
    model_template=`conf_getfile model_radar_template.grib`
    if [ -z "$model_template" ]; then
	echo "Error: grib template model_radar_template.grib for radar precipitation gridding not found in configuration directories" >&2
	return 1
    fi

    for nc in ../mosaico/SURF_RATE_????????????.nc; do
	if [ -f "$nc" ]; then
	    date=${nc%.nc}
	    date=${date#*/SURF_RATE_}
	    DATE=${date:0:8}
	    TIME=${date:8:4}
	    $RADAR_LHNDIR/netcdf2grib1_SIMC $nc $model_template
	    gribmosaico=radar_SRI_$date.grib1

	    if [ -f "$gribmosaico" ]; then
		if [ -n "$RADAR_LHN_GP" ]; then
		    grib_set -s generatingProcessIdentifier=$RADAR_LHN_GP \
			     $gribmosaico $gribmosaico.gp>/dev/null
		    mv -f $gribmosaico.gp $gribmosaico
		fi
		putarki_configured_setup $MODEL_SIGNAL "reftime=$date" "signal=$MODEL_SIGNAL format=grib"
		putarki_configured_archive $MODEL_SIGNAL $gribmosaico
		putarki_configured_end $MODEL_SIGNAL
	    fi
	fi
    done
}

simc_clean_radar_nc() {
    rm -f ../mosaico/COMP_????????????.nc
}

