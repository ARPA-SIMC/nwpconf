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
## @brief Module with functions specific to the SWAN model.
## @details This module provides functions specific to the sea wave
## model [SWAN](http://www.swan.tudelft.nl/). Grid-related functions
## for SWAN support only regular latlon grids.
## 
## The following environmental variables should be set by the user
## when using the functions from present module:
## 
## - `$SWAN_XMIN` indicates the minimum x coordinate of model domain
##   (longitude of lower left corner of the model grid)
## 
## - `$SWAN_XMAX` indicates the maximum x coordinate of model domain
##   (longitude of upper right corner of the model grid)
## 
## - `$SWAN_YMIN` indicates the minimum y coordinate of model domain
##   (latitude of lower left corner of the model grid)
## 
## - `$SWAN_YMAX` indicates the maximum y coordinate of model domain
##   (latitude of upper right corner of the model grid)
## 
## - `$SWAN_NX` indicates the number of intervals (number of points
##   -1) in the model grid along the x axis
## 
## - `$SWAN_NY` indicates the number of intervals (number of points
##   -1) in the model grid along the y axis

## @fn swan_model_init()
## @brief Setup the environment for SWAN model.
## @details This functions is implicitly called when the module is
## sourced, it sets up the environment for SWAN model with reasonable
## defaults.
swan_model_init() {

# arkimet parameter query
    MODEL_ARKI_PARAM="level:GRIB1,105,10;product:GRIB1,,2,33 or GRIB1,,2,34;"

# for retrieving data from MARS (predefined area, open ERA-interim dataset)
# base request (must not start with retrieve)
    MODEL_MARS_BASE=" dataset = interim,
 origin = all,
 type = an,
 area = `echo "$SWAN_YMAX+1"|bc`/`echo "$SWAN_XMIN-1"|bc`/`echo "$SWAN_YMIN-1"|bc`/`echo "$SWAN_YMAX+1"|bc`,
 grid = 0.5/0.5,"

# parameters request (must not start with retrieve)
    MODEL_MARS_PARAM=" levtype = sfc,
 param = 10u/10v"

# constant parameters request (must not start with retrieve)
    MODEL_MARS_CONST=" levtype = sfc,
 param = lsm"

# compute grid steps (note that $SWAN_NX/Y are 1 less than the number
# of grid points, Swan convention)

    SWAN_TDX=`echo "$SWAN_XMAX-$SWAN_XMIN"|bc`
    SWAN_TDY=`echo "$SWAN_YMAX-$SWAN_YMIN"|bc`
    SWAN_DX=`echo "scale=12;$SWAN_TDX/$SWAN_NX"|bc`
    SWAN_DY=`echo "scale=12;$SWAN_TDY/$SWAN_NY"|bc`

#    SWAN_CGRID="REGULAR $SWAN_XMIN $SWAN_YMIN 0. $SWAN_TDX $SWAN_TDY $SWAN_NX $SWAN_NY"
#    SWAN_INPGRID="REGULAR $SWAN_XMIN $SWAN_YMIN 0. $SWAN_NX $SWAN_NY $SWAN_DX $SWAN_DY"

}


## @fn inputmodel_name()
## @brief Output the filename corresponding to an input model analysis
## or boundary condition file.
## @details This function computes the filename of the input model
## analysis or boundary file according to the model convention, on
## the basis of `$PARENTMODEL` environment variable and all the
## variables related to the timing of the run. It a model-specific
## function for SWAN model, required by the getarki.sh module.
## @param $1 a=analysis numeric=boundary condition for the corresponding hour
inputmodel_name() {

    local suff
    if [ "$1" = "a" ]; then
	suff=${DATES}${TIMES}_000
    else
	suff=${DATES}${TIMES}_`printf "%03d" $1`
    fi

    echo ${PARENTMODEL}_wind_$suff

}


## @fn swan_compute_scanning_mode()
## @brief Output the number to be used in the SWAN command file
## according to the scanning mode of the input grib file
## @details This function scans the input grib file provided, reads
## the scanning mode of the first message and prints on stdout a
## number (either 2, 4 or 6) describing the grib scanning mode in the
## convention required by the SWAN command file as described in
## http://swanmodel.sourceforge.net/online_doc/swanuse/node26.html
## parameter \a idla of command \a READINP, e.g. for wind and
## bathymetry input. If the scanning mode is not handled by SWAN, 0 is
## printed.
## @param $1 name of the grib file in input
swan_compute_scanning_mode() {

    flag=(`grib_get -p iScansNegatively,jScansPositively,jPointsAreConsecutive $1|head -1`)
    if [ "${flag[0]}" = "0" ]; then
	if [ "${flag[1]}" = "0" -a "${flag[2]}" = "0" ]; then
	    echo "2"
	elif [ "${flag[1]}" = "1" -a "${flag[2]}" = "0" ]; then
	    echo "4"
	elif [ "${flag[1]}" = "1" -a "${flag[2]}" = "1" ]; then
	    echo "6"
	fi
    fi
}


## @fn swan_create_wind_input()
## @brief Generate an input file with wind field suitable for the SWAN
## model from an input file in grib format.
## @details This function interpolates all the 10m wind fields
## encountered in the input grib file on the grid defined for SWAN and
## outputs them in a text format suitable to be read by the SWAN
## model. The fields are interpolated on the configured latlon regular
## grid (other types of SWAN grids are not supported) according to
## `$SWAN_NX`, `$SWAN_XMIN` etc. environmental variables.  The input
## projections supported are those supported by libsim package; input
## wind components with different values of component flag (wind
## rotation) are supported. The input scanning mode (i.e. order of
## points in the grib fields) is assumed to be the same for all wind
## fields and an equivalent value in the SWAN convention is computed
## and assigned to the variable `$SWAN_WIND_SCMODE` to be used in the
## SWAN command file, see _swan_compute_scanning_mode()_ function.
## 
## The output text file should be read by SWAN using, in the command
## file, the key `EXC -99999.` in `INPGRID` command and the key
## `FORMAT "(19X,F17.0)"` in `READINP` command.
## 
## No check is made on the quantity and reference/forecast time of
## wind fields in input file, it is up to the user to make sure that
## they correspond to what SWAN expects.
## @param $1 input file name in grib format
## @param $2 output file name in text format
swan_create_wind_input() {

    local tmpout
    tmpout=wind_out.tmp
# --extrap to avoid missing values at the borders, use with care
    vg6d_transform --type=regular_ll --trans-type=inter --sub-type=bilin \
	--x-min=$SWAN_XMIN --y-min=$SWAN_YMIN --x-max=$SWAN_XMAX --y-max=$SWAN_YMAX \
	--nx=$(($SWAN_NX+1)) --ny=$(($SWAN_NY+1)) --component-flag=0 \
	$1 $tmpout
# consider to add:
# -F format, C style format for values. Default is "%.10e"
    grib_get_data -w shortName=10u/10v -m -99999. $tmpout > $2
    rm -f $tmpout
    export SWAN_WIND_SCMODE=`swan_compute_scanning_mode $1`

}


## @fn swan_create_bathymetry()
## @brief Generate an input file with bathymetry suitable for the SWAN
## model from an input file in grib format.
## @details This function interpolates the field in the input grib
## file on the grid defined for SWAN and outputs it in a text format
## suitable to be read by the SWAN model. The field is interpolated on
## the configured latlon regular grid (other types of SWAN grids are
## not supported) according to `$SWAN_NX`, `$SWAN_XMIN`
## etc. environmental variables.  The input projections supported are
## those supported by libsim package. The input scanning mode
## (i.e. order of points in the grib field) is read from the input
## file and an equivalent value in the SWAN convention is computed and
## assigned to the variable `$SWAN_BATHY_SCMODE` to be used in the
## SWAN command file, see _swan_compute_scanning_mode()_ function.
## 
## The output text file should be read by SWAN using, in the command
## file, the key `EXC -99999.` in `INPGRID` command and the key
## `FORMAT "(19X,F17.0)"` in `READINP` command.
## 
## The input grib file should contain only a single grib field with
## the height of earth surface/sea bottom above sea level in m, no
## check is made on the correctness of the parameter. Points having a
## missing value or a value greater than -0.1 m in the input file are
## set to a missing value (`EXC` key) in the output.
## 
## Notice that, if the field is coded as a topography height
## (i.e. with negative values in sea points), a value of `-1` for the
## `fac` key of the `READINP` command should be used in order to
## change the sign of the bathymetry values as expected by SWAN.
## @param $1 name of input file in grib format
## @param $2 name of output file in text format
swan_create_bathymetry() {

    local tmp1 tmp2
    tmp1=bathy1.tmp
    tmp2=bathy2.tmp

# interpolate from original bathymetry
    vg6d_transform --type=regular_ll --trans-type=inter --sub-type=bilin \
	--x-min=$SWAN_XMIN --y-min=$SWAN_YMIN --x-max=$SWAN_XMAX --y-max=$SWAN_YMAX \
	--nx=$(($SWAN_NX+1)) --ny=$(($SWAN_NY+1)) $1 $tmp1
# set "land" >-0.1m to missing
    vg6d_transform --trans-type=metamorphosis --sub-type=maskvalid \
	--maskbounds=-12000.,-0.1 --coord-file=$tmp1 --coord-format=grib_api $tmp1 $tmp2

# consider to add:
# -F format, C style format for values. Default is "%.10e"
    grib_get_data -m -99999. $tmp2 > $2
    rm -f $tmp1 $tmp2
    export SWAN_BATHY_SCMODE=`swan_compute_scanning_mode $1`

}

## @fn swan_make_grib()
## @brief Convert the SWAN gridded output into a file in grib format
## @details This function converts a set of plain text files,
## generated by the SWAN model on the computational regular latlon
## grid with a command like ``TABLE 'COMPGRID'...``, into a grib file
## using the ECMWF local table 140. The file name is assumed to be in
## the form `<dirname>/<prefix>_<param>_<suffix>` where `<prefix>`
## must not contain underscore characters and the value of `<param>`
## is used as the grib indicator of parameter for the variable in the
## file. It is assumed that each input text file contains data from
## start to end of run (`$DATES$TIMES` `$DATEE$TIMEE`) strictly at
## hourly intervals and the grib timerange is coded in forecast or
## analysis mode depending on the type of the run.
## @param $1-$n-1 name of input plain text files
## @param $n name of output grib file
swan_make_grib() {

# PYTHONPATH is ARPA/Fedora 20 specific setting
    PYTHONPATH=/usr/lib64/python2.7/site-packages/grib_api \
	$NWPCONFBINDIR/swan_make_grib.py "$@"

}

# start exporting all assignments
set -a
check_dep swan_model
# init module
swan_model_init
# stop exporting all assignments
set +a

