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
 area = 57/-32/24/50,
 grid = 0.5/0.5,"

# parameters request (must not start with retrieve)
    MODEL_MARS_PARAM=" levtype = sfc,
 param = 10u/10v"

# constant parameters request (must not start with retrieve)
    MODEL_MARS_CONST=" levtype = sfc,
 param = lsm"

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
	suff=${DATES}${TIMES}_000;;
    else
	suff=${DATES}${TIMES}_`printf "%03d" $1`
    fi

    echo ${PARENTMODEL}_wind_$suff

}

# start exporting all assignments
set -a
check_dep swan_model
# init module
swan_model_init
# stop exporting all assignments
set +a

