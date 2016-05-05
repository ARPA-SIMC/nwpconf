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

    flags=(`grib_get -p iScansNegatively,jScansPositively,jPointsAreConsecutive $1|head -1`)
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

}

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

}

# start exporting all assignments
set -a
check_dep swan_model
# init module
swan_model_init
# stop exporting all assignments
set +a

