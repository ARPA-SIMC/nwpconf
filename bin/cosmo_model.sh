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


# Delta time to be used in COSMO grib file names, input forecast time
# in h, output ddhh0000
timedelta_cosmo() {
    local d=0
    local h=$1
    while [ "$h" -ge 24 ]; do
	d=$(($d+1))
	h=$(($h-24))
    done
    printf "%02d%02d0000\n" $d $h
    
}

# $1 a=analysis numeric=boundary condition for hour n
inputmodel_name() {

    local pref suff
    if [ "$1" = "a" ]; then
        case "$INPUTMODEL" in
	COSMO*)
	    pref=laf
	    suff=$D1$T1;;
	GME*)
	    pref=giff
	    suff=00000000;;
	ICON*)
	    pref=igfff
	    suff=00000000;;
	IFS*)
	    pref=eas
	    suff=$D1$T1;;
	*)
	    pref=laf
	    suff=$D1$T1;;
    esac
    else
        case "$INPUTMODEL" in
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
    suff=`timedelta_cosmo $1`

    fi

    echo $pref$suff

}


# extract from arkimet archive the observations in bufr format
# required for the data assimilation in the proper time interval and
# convert them into COSMO-netcdf format, arguments are:
# $1 (optional) name of the logsim event to wait for, if empty it does
# not wait
getarki_obsncdf() {

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

