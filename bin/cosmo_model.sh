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



## @fn cosmo_model_init()
## @brief Setup the environment for COSMO model.
## @details This functions is implicitly called when the module is
## sourced, it sets up the environment for COSMO model with reasonable
## defaults.
cosmo_model_init() {
    READYFILE_PATTERN="LM[ABF]_*"

# configuration for continuous run
    MODEL_SOIL_PRODUCT="GRIB1,,201,197 or GRIB1,,201,198 or GRIB1,,2,85 or GRIB1,,2,51 or GRIB1,,201,200"
# T_SO,W_SO,T_S,QV_S,W_I
    MODEL_SNOW_PRODUCT="GRIB1,,2,65 or GRIB1,,201,203 or GRIB1,,201,129 or GRIB1,,201,133"
# W_SNOW,T_SNOW,FRESHSNW,RHO_SNOW

# definition of fields from parent model
    MODEL_ARKI_FROM_PARENT="
level:GRIB1,111 or GRIB1,1;
product:GRIB1,,2,8 or GRIB1,,2,81 or GRIB1,,2,83 or GRIB1,,2,87 or \
GRIB1,,202,46 or GRIB1,,202,47 or GRIB1,,202,48 or GRIB1,,202,49 or \
GRIB1,,202,61 or GRIB1,,202,62 or GRIB1,,202,64 or GRIB1,,202,65 or \
GRIB1,,202,75 or GRIB1,,202,76 or GRIB1,,202,57 or \
GRIB1,,202,84 or GRIB1,,202,86 or GRIB1,,202,91 or GRIB1,,202,92 or GRIB1,,202,93"
# HSURF,FR_LAND,Z0,PLCOV
# SSO_*4
# LAI,ROOTDP,HMO3,VIO3
# FOR_E,FOR_D,SOILTYP
# AER_*5
# soil and surface T and W moved here???
    MODEL_N_PARENT=10
    if [ "$MODEL_SOIL_PARENT" = Y ]; then
	MODEL_ARKI_FROM_PARENT="$MODEL_ARKI_FROM_PARENT or \
$MODEL_SOIL_PRODUCT"
	MODEL_N_PARENT=$(($MODEL_N_PARENT+8*2))
    fi
    if [ "$MODEL_SNOW_PARENT" = Y ]; then
	MODEL_ARKI_FROM_PARENT="$MODEL_ARKI_FROM_PARENT or \
$MODEL_SNOW_PRODUCT"
	MODEL_N_PARENT=$(($MODEL_N_PARENT+4))
    fi

# definition of slow fields from analysis
    MODEL_ARKI_FROM_ASSIM_SLOW="
level:GRIB1,111 or GRIB1,1;
product:GRIB1,,2,92 or GRIB1,,201,215"
# H_ICE,T_ICE
    MODEL_N_ASSIM_SLOW=2
    if [ "$MODEL_SOIL_PARENT" != Y ]; then
	MODEL_ARKI_FROM_ASSIM_SLOW="$MODEL_ARKI_FROM_ASSIM_SLOW or \
$MODEL_SOIL_PRODUCT"
	MODEL_N_ASSIM_SLOW=$(($MODEL_N_ASSIM_SLOW+8*2))
    fi
    if [ "$MODEL_SNOW_PARENT" != Y ]; then
	MODEL_ARKI_FROM_ASSIM_SLOW="$MODEL_ARKI_FROM_ASSIM_SLOW or \
$MODEL_SNOW_PRODUCT"
	MODEL_N_ASSIM_SLOW=$(($MODEL_N_ASSIM_SLOW+4))
    fi

# definition of fast fields from analysis
    MODEL_ARKI_FROM_ASSIM_FAST="level:GRIB1,109 or GRIB1,110;"
    MODEL_N_ASSIM_FAST=$(($MODEL_NLEV*4))

}

# Delta time to be used in COSMO grib file names, input forecast time
# in h, output ddhh0000
cosmo_timedelta() {
    local d=0
    local h=$1
    while [ "$h" -ge 24 ]; do
	d=$(($d+1))
	h=$(($h-24))
    done
    printf "%02d%02d0000\n" $d $h
    
}

## @fn inputmodel_name()
## @brief Output the filename corresponding to an input model analysis
## or boundary condition file.
## @details This function computes the filename of the input model
## analysis or boundary file according to the model convention, on
## the basis of `$PARENTMODEL` environment variable and all the
## variables related to the timing of the run. It a model-specific
## function for COSMO model, required by the getarki.sh module.
## @param $1 a=analysis numeric=boundary condition for the corresponding hour
inputmodel_name() {

    local pref suff
    if [ "$1" = "a" ]; then
        case "$PARENTMODEL" in
	COSMO*)
	    pref=laf
	    suff=$DATES$TIMES;;
	GME*)
	    pref=giff
	    suff=00000000;;
	ICON*)
	    pref=igfff
	    suff=00000000;;
	IFS*)
	    pref=eas
	    suff=$DATES$TIMES;;
	*)
	    pref=laf
	    suff=$DATES$TIMES;;
	esac
    else
        case "$PARENTMODEL" in
	COSMO*)
	    pref=lfff;;
	GME*)
	    pref=gfff;;
	ICON*)
	    pref=igfff;;
	IFS*)
	    pref=efsf;;
	*)
	    pref=lfff;;
	esac
	suff=`cosmo_timedelta $1`
    fi

    echo $pref$suff

}

## @fn model_readyfiletoname()
## @brief Output the filename(s) corresponding to a ready-file.
## @details This function takes the name of a ready-file and prints to
## stdout a shell pattern (possibly just a single name) representing
## the output files that can be generated correspondingly to the
## specified ready-file, according to the model convention. It a
## model-specific function for COSMO model, required by the putarki.sh
## module.
## @param $1 name of the ready-file
model_readyfiletoname() {

# extract time from the ready file
    local curtime=${1#LM[ABF]_}
    echo l??{t,f,c,}${curtime}{,c,p,z,s}

}


# extract from arkimet archive the observations in bufr format
# required for the data assimilation in the proper time interval and
# convert them into COSMO-netcdf format, arguments are:
# $1 (optional) name of the logsim event to wait for, if empty it does
# not wait
cosmo_getarki_obsncdf() {

# optional wait
    [ -n "$WAITFUNCTION" ] && $WAITFUNCTION
#    test -n "$1" && bufr_wait_logsim $1
    sms_meter_increment
# get data
    getarki_obsbufr obs_ecmwf.bufr
    sms_meter_increment

    if [ -s obs_ecmwf.bufr ]; then
# convert to netcdf
       	bufr_preconvert obs_ecmwf.bufr obs_wmo_cosmo.bufr
	sms_meter_increment
	bufr2netcdf -o obs obs_wmo_cosmo.bufr
	sms_meter_increment

# make symbolic links to files for COSMO
	make_ncdf_link . obs-0-0-13 cdfin_synop
	make_ncdf_link . obs-0-0-14 cdfin_synop_mob
	make_ncdf_link . obs-1-0-255 cdfin_ship
	make_ncdf_link . obs-2-4-255 cdfin_temp
	make_ncdf_link . obs-2-5-255 cdfin_tempship
# does not work at the moment, restore later
# should work since dballe-6.2-3961
#	make_ncdf_link . obs-2-1-4 cdfin_pilot
	make_ncdf_link . obs-2-1-5 cdfin_pilot_p
	make_ncdf_link . obs-4-0-8 cdfin_amdar
	make_ncdf_link . obs-4-0-9 cdfin_acars
    else
	exit 1
    fi
# create empty blacklist file
    touch blklsttmp

}


# template conversion for bufr, arguments are:
# $1 input bufr file (any template, typically ecmwf or wmo)
# $2 output bufr file (COSMO, WMO-like template)
bufr_preconvert() {

    dbamsg convert --bufr2netcdf-categories --template=wmo $1 > $2

}


# create symbolic links from files produced by bufr2netcdf to files
# for COSMO model, arguments are:
# $1 directory containing files
# $2 name of basic (first) netcdf file produced by bufr2netcdf
# (without .nc extension)
# $3 name of corresponding basic (first) COSMO file (without .nc
# extension)
make_ncdf_link() {
# link first file
    test -s $1/$2.nc && ln -sf $2.nc $1/$3.nc
# link following files if they exist
#    base=$1/${2%.nc}
    for file in $1/$2.*.nc; do
        if [ -s "$file" ]; then
            ext=${file#$1/$2}
            ln -sf `basename $file` $1/$3$ext
        else
            break
        fi
    done
}


## @fn cosmo_get_radar_lhn()
## @brief Retrieve precipitation files for LHN.
## @details This function retrieves from the configured from arkimet
## dataset the grib files with observed gridded precipitation
## (typically derived from radar data) required by COSMO for the
## latent heat nudging procedure. The time interval of data retrieved
## is computed on the basis of the environment variables defining
## assimilation and forecast time. The files are placed in the current
## directory with the name required by the model.
cosmo_get_radar_lhn() {

    local startdate enddate curdate nextdate
# when using data every 15' for some unperscrutable reason COSMO
# requires also the file for the previous hour

    startdate=`datetime_sub $DATES $TIMES 1`
    if [ "$DATE$TIME" = "$DATES$TIMES" ]; then # probably forecast
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

# start exporting all assignments
set -a
check_dep cosmo_model
# init module
cosmo_model_init
# stop exporting all assignments
set +a

