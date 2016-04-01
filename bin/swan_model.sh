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
 area = 46/-6/30/38,
 grid = 0.5/0.5,"

# parameters request (must not start with retrieve)
    MODEL_MARS_PARAM=" levtype = sfc,
 param = 10u/10v"

# constant parameters request (must not start with retrieve)
    MODEL_MARS_CONST=" levtype = sfc,
 param = lsm"

# compute grid steps (note that $SWAN_NX/Y are 1 less than the number
# of grid points, Swan convention)

    SWAN_TDX=`echo "scale=12;$SWAN_XMAX-$SWAN_XMIN"|bc`
    SWAN_TDY=`echo "scale=12;$SWAN_YMAX-$SWAN_YMIN"|bc`
    SWAN_DX=`echo "scale=12;$SWAN_TDX/$SWAN_NX"|bc`
    SWAN_DY=`echo "scale=12;$SWAN_TDY/$SWAN_NY"|bc`

    SWAN_CGRID="REGULAR $SWAN_XMIN $SWAN_YMIN 0. $SWAN_TDX $SWAN_TDY $SWAN_NX $SWAN_NY"
    SWAN_INPGRID="REGULAR $SWAN_XMIN $SWAN_YMIN 0. $SWAN_NX $SWAN_NY $SWAN_DX $SWAN_DY"

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


swan_create_wind_input() {

    local tmpout
    tmpout=wind_out.tmp

    vg6d_transform --type=regular_ll --trans-type=inter --sub-type=bilin \
	--x-min=$SWAN_XMIN --y-min=$SWAN_YMIN --x-max=$SWAN_XMAX --y-max=$SWAN_YMAX \
	--nx=$(($SWAN_NX+1)) --ny=$(($SWAN_NY+1)) $1 $tmpout
# consider to add:
# -m missingValue, Default is to skip the missing values.
# -F format, C style format for values. Default is "%.10e"
    grib_get_data -w shortName=10u $tmpout > $2
    grib_get_data -w shortName=10v $tmpout >> $2
    rm -f $tmpout

}

# start exporting all assignments
set -a
check_dep swan_model
# init module
swan_model_init
# stop exporting all assignments
set +a

