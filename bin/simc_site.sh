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
	ds=`getarki_datetime $d $t`
    else
	ds=`getarki_datetime $DATE $TIME`
    fi

    simc_wait_logevent "" "$ds" $1

}



# Wait for an event in the SIMC logging system.
# @param $1 specific message to wait for within an event (if empty any message will match)
# @param $1 nominal date and time of event (`+%Y-%m-%d %H:%M`, UTC), 
# @param $3 name of event
simc_wait_logevent() {

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
	sleep $PUTARKI_WAITSTART
    done
# password in ~/.pgpass
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
# 
# @param $1 optional start date and time (`+%Y%m%d%H%M`, UTC)
# @param $2 optional end date and time (`+%Y%m%d%H%M`, UTC)
simc_create_radar_grib() {
    local model_template defaultdate fromdate todate succdate ncmosaico gribmosaico

    succdate=""
# get grib template on the model grid
    model_template=`conf_getfile model_radar_template.grb`
    if [ -z "$model_template" ]; then
	echo "Error: grib template model_radar_template.grib for radar precipitation gridding not found in configuration directories"
	return 1
    fi

    fromdate=$1
    todate=$2
   
# loop over radar-precipitation-rate time levels
    while [ "$fromdate" -le "$todate" ]; do
# compute filenames for current date
	ncmosaico=COMP_$fromdate.nc
	gribmosaico=radar_SRI_$fromdate.grib1

# create mosaico in netcdf format ($ncmosaico) from SIMC archives
	$RADAR_MOSAICODIR/Mosaico.sh $RADAR_MOSAICODIR/mosaico.config -dn $fromdate -C $RADAR_MOSAICOCONF>/dev/null 2>&1

	if [ -f $ncmosaico ]; then 
	    rm -f $gribmosaico
# create precipitation grib file ($gribmosaico) from netcdf
# ($ncmosaico) for current date and archive
	    $RADAR_LHNDIR/netcdf2grib1_SIMC $ncmosaico $model_template>/dev/null 2>&1
	    if [ -f $gribmosaico ]; then
		if [ -n "$RADAR_LHN_GP" ]; then
		    grib_set -s generatingProcessIdentifier=$RADAR_LHN_GP \
			$gribmosaico $gribmosaico.gp
		    mv -f $gribmosaico.gp $gribmosaico
		fi
		putarki_archive grib $gribmosaico
	    fi
	    rm -f $ncmosaico $gribmosaico
# store successful date
	    succdate=$fromdate
	fi

# increment date
	fromdate=$(date -u --date "${fromdate:0:8} ${fromdate:8:4} $RADAR_DT minutes" "+%Y%m%d%H%M")
    done
    # output last processed date
    echo $succdate
}


simc_get_radar_lhn() {

    local startdate enddate curdate nextdate
# when using data every 15' for some unperscrutable reason COSMO
# requires also the file for the previous hour

    startdate=`datetime_sub $D1 $T1 1`
    if [ "$DATE$TIME" = "$D1$T1" ]; then # probably forecast
	enddate=`datetime_add $DATE $TIME 3`
    else # assimilation
	enddate=`datetime_add $DATE $TIME 1`
    fi

    echo "$startdate:$enddate"
    curdate=$startdate
    while [ "$curdate" -le "$enddate" ]; do

	nextdate=`datetime_add $curdate 1`
	echo "$nextdate"
	filename=${curdate:2}.grib1 # 2-digit year
	arki-query --data -o $filename "Reftime:>=`getarki_datetime $curdate` <`getarki_datetime $nextdate`" $ARKI_LHN_DS
	 [ -f $filename ] || touch $filename

	curdate=$nextdate
    done

}
