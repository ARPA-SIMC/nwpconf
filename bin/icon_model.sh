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



## @fn icon_model_init()
## @brief Setup the environment for ICON model.
## @details This functions is implicitly called when the module is
## sourced, it sets up the environment for ICON model with reasonable
## defaults.
icon_model_init() {

    READYFILE_PATTERN="*.rf"
# for retrieving data from MARS (predefined area, open ERA-interim dataset)
# base request (must not start with retrieve)
    MODEL_MARS_BASE=" dataset = interim,
 origin = all,
 area = 57/-32/24/50,
 grid = 0.5/0.5,"
# removed type = an,

# parameters request (must not start with retrieve)
    MODEL_MARS_PARAM=" levelist = all,
 levtype = ml,
 param = u/v/w/t/q/clwc/ciwc/lnsp
retrieve,
 levtype = pl,
 levelist = 200,
 param = z
retrieve,
 levtype = sfc,
 param = skt/tsn/sd/src/stl1/stl2/stl3/stl4/swvl1/swvl2/swvl3/swvl4/ci/istl1"

# constant parameters request (must not start with retrieve)
    MODEL_MARS_CONST=" levtype = ml,
 levelist = 1,
 param = z
retrieve,
 levtype = sfc,
 param = z/lsm/slt"

# ensemble prediction
    ENS_MODE=.FALSE.
    if [ -n "$ENS_TOTAL_MEMB" -a -n "$ENS_MEMB" ]; then
	ENS_MODE=.TRUE.
    fi

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
    if [ "$MODEL_LAKE" = Y ]; then
        MODEL_ARKI_FROM_PARENT="$MODEL_ARKI_FROM_PARENT or \
GRIB1,,202,55 or GRIB1,,201,96"
# FR_LAKE,DEPTH_LK
#       MODEL_N_PARENT=$(($MODEL_N_PARENT+2))
    fi

# definition of slow fields from analysis
    MODEL_ARKI_FROM_ASSIM_SLOW1="product:GRIB2,,2,3,18 or GRIB2,,0,0,17 or GRIB2,,10,2,8 or GRIB2,,10,2,1 or GRIB2,,0,0,18 or GRIB2,,0,1,11 or GRIB2,,0,1,60 or GRIB2,,0,1,61, or GRIB2,,0,1,42 or GRIB2,,0,1,203 or GRIB2,,2,0,13 or GRIB2,,2,3,20 or GRIB2,,2,3,22 or GRIB2,,2,3,18 or GRIB2,,1,2,1 or GRIB2,,1,2,10 or GRIB2,,1,2,0 or GRIB2,,1,2,4 or GRIB2,,1,2,3"
    MODEL_ARKI_FROM_ASSIM_SLOW2="level:GRIB2S,1; product:GRIB2,,0,0,0 or GRIB2,,0,1,0"
    # SMI: product:GRIB2,,2,3,200
    MODEL_N_ASSIM_SLOW=40 	# should be 45, but we round to 40
}


## @fn inputmodel_name()
## @brief Output the filename corresponding to an input model analysis
## or boundary condition file.
## @details This function computes the filename of the input model
## analysis or boundary file according to the model convention, on
## the basis of `$PARENTMODEL` environment variable and all the
## variables related to the timing of the run. It a model-specific
## function for ICON model, required by the getarki.sh module.
## @param $1 a=analysis numeric=boundary condition for the corresponding hour
inputmodel_name() {

    local pref suff
    if [ "$1" = "a" ]; then
        case "$PARENTMODEL" in
	COSMO*)
	    pref=laf
	    suff=$DATES$TIMES;;
	ICON*)
	    pref=igfff
	    suff=00000000;;
	IFS*)
	    pref=eas_$DATES$TIMES
	    suff=+00.grb;;
	*)
	    pref=laf
	    suff=$DATES$TIMES;;
	esac
    else
        case "$PARENTMODEL" in
	COSMO*)
	    pref=lfff;;
	ICON*)
	    pref=igfff;;
	IFS*)
	    pref=efs_$DATES$TIMES;;
	*)
	    pref=lfff;;
	esac
	suff=`printf "+%02d.grib" $1`
    fi

    echo $pref$suff

}

## @fn model_readyfiletoname()
## @brief Output the filename(s) corresponding to a ready-file.
## @details This function takes the name of a ready-file and prints to
## stdout a shell pattern (possibly just a single name) representing
## the output files that can be generated correspondingly to the
## specified ready-file, according to the model convention. It a
## model-specific function for ICON model, required by the putarki.sh
## module.
## @param $1 name of the ready-file
model_readyfiletoname() {

    # extract reftime and delta from the ready file
    # <nomerun>_<YYYYmmddHHMM>_<tipocampo>_<tipogriglia>_<tipopp>_+<ddHHMMSS>.grb
    # <nomerun>_<YYYYmmddHHMM>_+<ddHHMMSS>.rf
    f=${1%.rf}
    delta=${f#*_+}
    nreftime=${f%_+$delta}
    echo ${nreftime}_*_*_*_+${delta}.grb

}


## @fn model_readyfiletosignal()
## @brief Output a signal name corresponding to a ready-file.
## @details This function takes the name of a ready-file and prints to
## stdout a name to be used for signalling the advance of analysis or forecast.
## @param $1 name of the ready-file
model_readyfiletosignal() {

    local signalname

    if [ "$MODEL_BACK" -gt 0 ]; then # assimilation run
	signalname=${1#LMA_}
	if [ "$signalname" != "$1" ]; then
	    echo "$signalname"
	fi
#    else # forecast run
#	signalname=${1#LMF_}
#	if [ "$signalname" != "$1" ]; then
#	    echo "$signalname"
#	fi
    fi

}


## @fn icon_getarki_obsncdf()
## @brief Retrieve observation data for assimilation.
## @details This function retrieves from the arkimet dataset specified
## by `$BUFR_ARKI_DS_CONV` and `$BUFR_ARKI_DS_NOCONV` (see
## getarki.sh::getarki_obsbufr() function) the observations in bufr 
## format required by COSMO and converts them into COSMO-netcdf format.
## The files are placed in the current directory with the name required 
## by the model. The time interval of data retrieved is computed on the
## basis of the environment ## variables defining assimilation and 
## forecast time.
## @param $1 (optional) name of the logsim event to wait for, if empty it does not wait
icon_getarki_obsncdf() {

# optional wait
    [ -n "$WAITFUNCTION" ] && $WAITFUNCTION
#    test -n "$1" && bufr_wait_logsim $1
    type meter_increment 2>/dev/null && meter_increment || true
# get data
    getarki_obsbufr obs_ecmwf_conv.bufr obs_wmo_cosmo_noconv.bufr 1
    type meter_increment 2>/dev/null && meter_increment || true

    # convert to netcdf
    if [ -s obs_ecmwf_conv.bufr ] || [ -s obs_wmo_cosmo_noconv.bufr ]; then
        if [ -s obs_ecmwf_conv.bufr ] && [ -s obs_wmo_cosmo_noconv.bufr ]; then
            bufr_preconvert obs_ecmwf_conv.bufr obs_wmo_cosmo_conv.bufr
            cat obs_wmo_cosmo_conv.bufr obs_wmo_cosmo_noconv.bufr > obs_wmo_cosmo.bufr
        elif [ ! -s obs_ecmwf_conv.bufr ] && [ -s obs_wmo_cosmo_noconv.bufr ]; then
            mv obs_wmo_cosmo_noconv.bufr obs_wmo_cosmo.bufr
        elif [ -s obs_ecmwf_conv.bufr ] && [ ! -s obs_wmo_cosmo_noconv.bufr ]; then
            bufr_preconvert obs_ecmwf_conv.bufr obs_wmo_cosmo.bufr
        fi

        type meter_increment 2>/dev/null && meter_increment || true
        $SIMC_TOOLS bufr2netcdf -o obs obs_wmo_cosmo.bufr
        type meter_increment 2>/dev/null && meter_increment || true

        # make symbolic links to files for COSMO
#        make_ncdf_link . obs-0-0-13 cdfin_synop
#        make_ncdf_link . obs-0-0-14 cdfin_synop_mob
#        make_ncdf_link . obs-1-0-255 cdfin_ship
#        make_ncdf_link . obs-2-4-255 cdfin_temp
#        make_ncdf_link . obs-2-5-255 cdfin_tempship
        # does not work at the moment, restore later
        # should work since dballe-6.2-3961
        #   make_ncdf_link . obs-2-1-4 cdfin_pilot
#        make_ncdf_link . obs-2-1-5 cdfin_pilot_p
        #make_ncdf_link . obs-4-0-8 cdfin_amdar     # if template is converted
#        make_ncdf_link . obs-4-255-146 cdfin_amdar
#        make_ncdf_link . obs-4-0-9 cdfin_acars

        make_ncdf_link2 . cdfin_synop obs-0-0-13 obs-0-255-170 obs-0-255-172 obs-0-255-176 obs-0-255-178
        make_ncdf_link2 . cdfin_synop_mob obs-0-0-14
        make_ncdf_link2 . cdfin_ship obs-1-0-255 obs-1-255-180
        make_ncdf_link2 . cdfin_temp obs-2-4-255 obs-2-255-109 obs-2-255-111
        make_ncdf_link2 . cdfin_tempship obs-2-5-255
        # does not work at the moment, restore later
        # should work since dballe-6.2-3961
        make_ncdf_link2 . cdfin_pilot obs-2-1-4
        make_ncdf_link2 . cdfin_pilot_p obs-2-1-5
        make_ncdf_link2 . cdfin_amdar obs-4-0-8 obs-4-255-146
        make_ncdf_link2 . cdfin_acars obs-4-0-9

    else
        rm -f obs_ecmwf_conv.bufr obs_wmo_cosmo_noconv.bufr
        touch noobs
    fi
# create empty blacklist file
    touch blklsttmp

}


# template conversion for bufr, arguments are:
# $1 input bufr file (any template, typically ecmwf or wmo)
# $2 output bufr file (COSMO, WMO-like template)
bufr_preconvert() {

    $SIMC_TOOLS dbamsg convert --bufr2netcdf-categories --template=wmo $1 > $2

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


# create symbolic links from files produced by bufr2netcdf to files
# for COSMO model, arguments are:
# $1 directory containing files
# $2 name of basic (first) COSMO file (without .nc extension)
# $3-$n name of each basic (first) netcdf file produced by bufr2netcdf
# (without .nc extension)
make_ncdf_link2() {
    local dir=$1
    local ofile=$2
    shift; shift
    local ext=''
    for filetype in "$@"; do
	for file in $dir/$filetype.nc $dir/$filetype.*.nc; do
	    if [ -s $file ]; then
		ln -sf $file $dir/$ofile$ext.nc
		if [ -z "$ext" ]; then
		    ext=.2
		else
		    ext=.$((${ext#.}+1))
		fi
	    fi
	done
    done
}


## @fn icon_getarki_lhn()
## @brief Retrieve precipitation data for LHN.
## @details This function retrieves from the arkimet dataset specified
## by `$ARKI_LHN_DS` the grib files with observed gridded
## precipitation (typically derived from radar data) required by COSMO
## for the latent heat nudging procedure. The time interval of data
## retrieved is computed on the basis of the environment variables
## defining assimilation and forecast time. The files are placed in
## the current directory with the name required by the model.
icon_getarki_lhn() {

    local startdate enddate curdate nextdate
    # Set MODEL_NH_LHN
    if [ -z ${MODEL_NH_LHN} ]; then
        MODEL_NH_LHN=3
    fi

    # Define time interval
    startdate=`datetime_sub $DATES $TIMES 1`
    if [ "$DATE$TIME" = "$DATES$TIMES" ]; then # probably forecast
        enddate=`datetime_add $DATE $TIME $MODEL_NH_LHN`
    else # assimilation
        enddate=`datetime_add $DATE $TIME 1`
    fi
    echo "$startdate:$enddate"
    curdate=$startdate
    while [ "$curdate" -le "$enddate" ]; do

	nextdate=`datetime_add $curdate 1`
	echo "$nextdate"
	filename=${curdate:2}.grib # 2-digit year
	arki-query --data -o $filename "Reftime:>=`getarki_datetime $curdate`,<`getarki_datetime $nextdate`" $ARKI_LHN_DS
	 [ -f $filename ] || touch $filename
# depending on model version .grib or .grib1 may be required
        ln -fs $filename ${filename}1

	curdate=$nextdate
    done

}

# start exporting all assignments
set -a
check_dep cosmo_model
# init module
icon_model_init
# stop exporting all assignments
set +a

