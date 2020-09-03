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
## @brief Module with functions performing postprocessing tasks for
## LAMI operational model runs.
## @details This module provides functions that perform mainly
## area-cutting postprocessing tasks used in various stages of the
## LAMI operational suites.

## @fn lami_make_itr()
## @brief Cut a grib file to itr area.
## @details Cut a grib file in order to obtain an area covering strictly
## Italian territory, possibly excluding some small islands, as in the
## lama dataset, tuned for the cosmo-5M grid.
## @param $1 input grib file
## @param $2 output grib file
lami_make_itr()
{
    log "start make_itr $1"
    # area itr (~"lama")
    time vg6d_transform --trans-mode=s --trans-type=zoom --sub-type=coord \
        --ilon=3.6 --ilat=33.8 --flon=23.5 --flat=49. \
        $1 $2
    POSTPROC_FORMAT=grib
    log "end make_itr"
}

## @fn lami_make_nit()
## @brief Cut a grib file to nit area.
## @details Cut a grib file in order to obtain an area covering Northern
## Italy, tuned for the cosmo-2I grid.
## @param $1 input grib file
## @param $2 output grib file
lami_make_nit()
{
    log "start make_nit $1"
    # area nit, nord Italia dal cosmo 2I con conservazione dello staggering
    time vg6d_transform --trans-mode=s --trans-type=zoom --sub-type=coord \
        --ilon=5.61 --ilat=42.52 --flon=18. --flat=48.01 \
        $1 $2
    POSTPROC_FORMAT=grib
    log "end make_nit"
}


## @fn lami_make_medl()
## @brief Reduce resolution of Mediterranean grid.
## @details Reduce by 4 times the linear resolution of the grib file,
## keeping the overall area, tuned for the cosmo-5M grid on the whole
## Mediterranean area. After this operation the staggering of the grid
## will be wrong.
## @param $1 input grib file
## @param $2 output grib file
lami_make_medl()
{
    log "start make_medl $1"
    time vg6d_transform --trans-mode=s --trans-type=zoom --sub-type=index \
        --ix=1 --iy=4 --fx=1083 --fy=559 ${1} tmp.$$
    vg6d_transform --trans-mode=s \
        --trans-type=boxregrid --sub-type=average --npx=4 --npy=4 \
        tmp.$$ $2
    rm -f tmp.$$
    POSTPROC_FORMAT=grib
    log "end make_medl"
}


## @fn lami_make_vprof()
## @brief Generate vertical profiles on selected points.
## @details Interpolate a grib file on a fixed list of geographical
## points in order to obtain vertical profiles of various quantities
## as pseudo-soundings in bufr format.
## @param $1 input grib file
## @param $2 output bufr file
lami_make_vprof()
{
     log "start make_vprof $1"
# almost equivalent with grib_copy (need to exclude qi)
#    grib_copy -w indicatorOfParameter=40,indicatorOfTypeOfLevel=109 $1 ${1}_109
#    grib_copy -w indicatorOfParameter=1/33/34/11/17/51,indicatorOfTypeOfLevel=110 $1 ${1}_110

    # select data on half levels
    arki-query --data -o ${1}_109 \
        'level:GRIB1,109; product:GRIB1,,2,40;' \
        grib:$1
    # select data on full levels
    arki-query --data -o ${1}_110 \
        'level:GRIB1,110; product:GRIB1,,2,33 or GRIB1,,2,34 or GRIB1,,2,11 or GRIB1,,2,17 or GRIB1,,2,51 or GRIB1,,2,1;' \
        grib:$1

    # vertical interpolation to full levels
    vg6d_transform --component-flag=1 --trans-type=vertint --sub-type=linear \
        --trans-level-type=105,,105,105 \
        ${1}_109 ${1}_109_110
    cat ${1}_109_110 >> ${1}_110
    # retrieve previously saved static data if available (improve)
    cat $HOME/static/$2/last_110.grib >> ${1}_110 || true
    # destaggering of u and v and extension of height (B10007) to all
    # time levels
    vg6d_transform --a-grid --anavariable-list=B10007 ${1}_110 ${1}_destag
    # interpolation on points | computation of derived variables
    # improve management of coordinates
    vg6d_getpoint --output-format=native --network=temp \
        --lon=7.32000,7.76600,6.96600,7.85000,9.66667,10.33333,10.91667,10.60000,11.33333,12.05000,11.58333,12.20000,12.56667,13.00000,15.00000,9.00000,13.45250,7.61300,6.95000,8.80000,16.03333,13.18333,7.65000,9.28333,11.85000,8.85000,9.93333,10.70000,11.00000,11.61667,10.38333,12.43333,17.95000,12.50000,9.06667,7.66000,8.66000,8.60000,6.81000,7.06500,8.49000,8.30000,8.53900,9.32900,8.59600,9.11500,9.50100,8.53600,9.54200,12.56700,12.25000,11.78300,11.53300,11.00000,12.21700,11.88300,9.1881263,9.6687071,10.2229390,9.0854556,10.0261350,9.3900705,9.4978501,10.7976976,9.2730143,9.1566316,9.8693336,8.8263844,9.0097,9.2169,9.3717 \
        --lat=45.73700,45.60000,45.78300,45.47000,45.02775,44.80000,44.66667,44.71667,44.48333,44.21667,44.83333,44.41667,44.06667,45.00000,43.00000,44.00000,43.29920,44.53900,46.81667,41.91667,45.81667,46.03333,45.21667,45.44442,45.40000,44.41667,44.44442,44.21108,44.02775,44.65000,43.68333,41.65000,40.65000,37.91667,39.25000,45.00000,44.90000,45.90000,44.95000,45.14000,45.49000,46.12000,40.74300,40.32500,39.90100,39.22600,40.92400,39.31100,39.88000,45.58300,45.66700,45.08700,45.55000,45.43300,46.13300,45.41700,45.4636707,45.6947359,45.5397733,45.8119642,45.1334974,45.8529825,45.3128778,45.1603653,45.5840057,45.1858767,46.1712597,45.8176046,44.9936,45.8108,46.1372 \
        ${1}_destag - | \
        v7d_transform --input-format=native --output-format=BUFR  \
        --output-variable-list=B10007,B10004,B11001,B11002,B11003,B11004,B11006,B12101,B12103,B13001,B13003 \
        - ${2}

    rm -f ${1}_109 ${1}_110 ${1}_109_110 ${1}_destag
    POSTPROC_FORMAT=bufr
    log "end make_vprof"
}


# start exporting all assignments
#set -a
#check_dep
#check_defined
# stop exporting all assignments
#set +a

