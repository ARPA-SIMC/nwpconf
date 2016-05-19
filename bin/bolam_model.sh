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

## @fn inputmodel_name()
## @brief Output the filename corresponding to an input model analysis
## or boundary condition file.
## @details This function computes the filename of the input model
## analysis or boundary file according to the model convention, on
## the basis of `$PARENTMODEL` environment variable and all the
## variables related to the timing of the run. It a model-specific
## function for BOLAM model, required by the getarki.sh module.
## @param $1 a=analysis numeric=boundary condition for the corresponding hour
inputmodel_name() {

    local pref suff
    case "$PARENTMODEL" in
	IFS*)
	    pref=IFS;;
	GFS*)
	    pref=GFS;;
	*)
	    pref=INP;;
    esac
    if [ "$1" = "a" ]; then
	suff=000
    else
	suff=`printf '%03d' $1`
    fi
    echo ${pref}_${DATES}${TIMES}+${suff}.grib

}


## @fn bolam_model_init()
## @brief Setup the environment for BOLAM model.
## @details This functions is implicitly called when the module is
## sourced, it sets up the environment for BOLAM model with reasonable
## defaults.
bolam_model_init() {

# for retrieving data from MARS (predefined area, open ERA-interim dataset)
# base request (must not start with retrieve)
    MODEL_MARS_BASE=" dataset = interim,
 origin = all,
 type = an,
 area = 57/-32/24/50,
 grid = 0.5/0.5,"

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

}
