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
    time $SIMC_TOOLS vg6d_transform --trans-mode=s --trans-type=zoom --sub-type=coord \
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
    time $SIMC_TOOLS vg6d_transform --trans-mode=s --trans-type=zoom --sub-type=coord \
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
    local tmpfile=`dirname $2`/tmp.$$
    time $SIMC_TOOLS vg6d_transform --trans-mode=s --trans-type=zoom --sub-type=index \
        --ix=1 --iy=4 --fx=1083 --fy=559 ${1} $tmpfile
    $SIMC_TOOLS vg6d_transform --trans-mode=s \
        --trans-type=boxregrid --sub-type=average --npx=4 --npy=4 \
        $tmpfile $2
    rm -f $tmpfile
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
     local tmpdir=`dirname $2`
# almost equivalent with grib_copy (need to exclude qi)
#    grib_copy -w indicatorOfParameter=40,indicatorOfTypeOfLevel=109 $1 ${1}_109
#    grib_copy -w indicatorOfParameter=1/33/34/11/17/51,indicatorOfTypeOfLevel=110 $1 ${1}_110

    # select data on half levels
    $SIMC_TOOLS arki-query --data -o $tmpdir/${1}_109 \
        'level:GRIB1,109; product:GRIB1,,2,40;' \
        grib:$1
    # select data on full levels
    $SIMC_TOOLS arki-query --data -o $tmpdir/${1}_110 \
        'level:GRIB1,110; product:GRIB1,,2,33 or GRIB1,,2,34 or GRIB1,,2,11 or GRIB1,,2,17 or GRIB1,,2,51 or GRIB1,,2,1;' \
        grib:$1
    if [ -s "$tmpdir/${1}_109" ]; then

    # vertical interpolation to full levels
    $SIMC_TOOLS vg6d_transform --component-flag=1 --trans-type=vertint --sub-type=linear \
        --trans-level-type=105,,105,105 \
        $tmpdir/${1}_109 $tmpdir/${1}_109_110
    cat $tmpdir/${1}_109_110 >> $tmpdir/${1}_110
    # retrieve previously saved static data if available (improve)
    cat $MODEL_STATIC/$MODEL_SIGNAL/last_hfl.grib >> $tmpdir/${1}_110 || true
    # destaggering of u and v and extension of height (B10007) to all
    # time levels
    $SIMC_TOOLS vg6d_transform --a-grid --anavariable-list=B10007 $tmpdir/${1}_110 $tmpdir/${1}_destag
    # interpolation on points | computation of derived variables
    # improve management of coordinates
    $SIMC_TOOLS vg6d_getpoint --output-format=native --network=${VPROF_NETWORK:-temp} \
        --lon=7.32000,7.76600,6.96600,7.85000,9.66667,10.33333,10.91667,10.60000,11.33333,12.05000,11.58333,12.20000,12.56667,13.00000,15.00000,9.00000,13.45250,7.61300,6.95000,8.80000,16.03333,13.18333,7.65000,9.28333,11.85000,8.85000,9.93333,10.70000,11.00000,11.61667,10.38333,12.43333,17.95000,12.50000,9.06667,7.66000,8.66000,8.60000,6.81000,7.06500,8.49000,8.30000,8.53900,9.32900,8.59600,9.11500,9.50100,8.53600,9.54200,12.56700,12.25000,11.78300,11.53300,11.00000,12.21700,11.88300,9.1881263,9.6687071,10.2229390,9.0854556,10.0261350,9.3900705,9.4978501,10.7976976,9.2730143,9.1566316,9.8693336,8.8263844,9.0097,9.2169,9.3717,10.73925875,11.03701917,10.72769104,10.89391867,11.14815314,11.12163115,10.99505332,11.77270212,11.46392998,11.63576284,11.46517292,11.23720428 \
        --lat=45.73700,45.60000,45.78300,45.47000,45.02775,44.80000,44.66667,44.71667,44.48333,44.21667,44.83333,44.41667,44.06667,45.00000,43.00000,44.00000,43.29920,44.53900,46.81667,41.91667,45.81667,46.03333,45.21667,45.44442,45.40000,44.41667,44.44442,44.21108,44.02775,44.65000,43.68333,41.65000,40.65000,37.91667,39.25000,45.00000,44.90000,45.90000,44.95000,45.14000,45.49000,46.12000,40.74300,40.32500,39.90100,39.22600,40.92400,39.31100,39.88000,45.58300,45.66700,45.08700,45.55000,45.43300,46.13300,45.41700,45.4636707,45.6947359,45.5397733,45.8119642,45.1334974,45.8529825,45.3128778,45.1603653,45.5840057,45.1858767,46.1712597,45.8176046,44.9936,45.8108,46.1372,46.30954672,46.36651869,46.03791191,45.91092998,46.21847190,46.07286466,45.75644409,46.47591703,46.28929420,46.01432245,46.05516379,46.06234010 \
        $tmpdir/${1}_destag $tmpdir/${1}_point.v7d #- |
        $SIMC_TOOLS v7d_transform --input-format=native --output-format=BUFR  \
        --output-variable-list=B10007,B10004,B11001,B11002,B11003,B11004,B11006,B12101,B12102,B12103,B13001,B13003 \
        $tmpdir/${1}_point.v7d ${2}
    fi
    rm -f $tmpdir/${1}_109 $tmpdir/${1}_110 $tmpdir/${1}_109_110 $tmpdir/${1}_destag $tmpdir/${1}_point.v7d
    POSTPROC_FORMAT=bufr
    log "end make_vprof"
}

## @fn lami_make_arkiruc()
## @brief Filter from a grib file a specified subset of variables.
## @details Filter from the input file a subset of the available
## variables suitable for permanent archiving, tuned for the
## cosmo_2I_fcruc dataset.
## @param $1 input grib file
## @param $2 output grib file
lami_make_arkiruc()
{
    log "start make_arkiruc $1"
    $SIMC_TOOLS arki-query --data -o $2 \
	"level:GRIB1,1 or GRIB1,2 or GRIB1,3 or GRIB1,4 or GRIB1,8 or GRIB1,102 or GRIB1,105 or GRIB1,111 or GRIB1,112;timerange:GRIB1,0,0 or GRIB1,0,1h,0h or GRIB1,0,2h,0h or GRIB1,0,3h,0h or GRIB1,0,4h,0h or GRIB1,0,5h,0h or GRIB1,0,6h,0h or GRIB1,0,7h,0h or GRIB1,0,8h,0h or GRIB1,0,9h,0h or GRIB1,0,10h,0h or GRIB1,0,11h,0h or GRIB1,0,12h,0h or GRIB1,0,13h,0h or GRIB1,0,14h,0h or GRIB1,0,15h,0h or GRIB1,0,16h,0h or GRIB1,0,17h,0h or GRIB1,0,18h,0h or GRIB1,2,0h,1h or GRIB1,2,1h,2h or GRIB1,2,2h,3h or GRIB1,2,3h,4h or GRIB1,2,4h,5h or GRIB1,2,5h,6h or GRIB1,2,6h,7h or GRIB1,2,7h,8h or GRIB1,2,8h,9h or GRIB1,2,9h,10h or GRIB1,2,10h,11h or GRIB1,2,11h,12h or GRIB1,2,12h,13h or GRIB1,2,13h,14h or GRIB1,2,14h,15h or GRIB1,2,15h,16h or GRIB1,2,16h,17h or GRIB1,2,17h,18h or GRIB1,3,0h,1h or GRIB1,3,0h,2h or GRIB1,3,0h,3h or GRIB1,3,0h,4h or GRIB1,3,0h,5h or GRIB1,3,0h,6h or GRIB1,3,0h,7h or GRIB1,3,0h,8h or GRIB1,3,0h,9h or GRIB1,3,0h,10h or GRIB1,3,0h,11h or GRIB1,3,0h,12h or GRIB1,3,0h,13h or GRIB1,3,0h,14h or GRIB1,3,0h,15h or GRIB1,3,0h,16h or GRIB1,3,0h,17h or GRIB1,3,0h,18h or GRIB1,4,0h,1h or GRIB1,4,0h,2h or GRIB1,4,0h,3h or GRIB1,4,0h,4h or GRIB1,4,0h,5h or GRIB1,4,0h,6h or GRIB1,4,0h,7h or GRIB1,4,0h,8h or GRIB1,4,0h,9h or GRIB1,4,0h,10h or GRIB1,4,0h,11h or GRIB1,4,0h,12h or GRIB1,4,0h,13h or GRIB1,4,0h,14h or GRIB1,4,0h,15h or GRIB1,4,0h,16h or GRIB1,4,0h,17h or GRIB1,4,0h,18h" grib:$1
    POSTPROC_FORMAT=grib
    log "end make_arkiruc"
}

## @fn lami_make_arki5I()
## @brief Filter from a grib file a specified subset of variables.
## @details Filter from the input file a subset of the available
## variables suitable for permanent archiving, tuned for the
## cosmo_5I dataset.
## @param $1 input grib file
## @param $2 output grib file
lami_make_arki5I()
{
    log "start make_arki5I $1"
    rm -f $2
    # max/min - (near)surf
    $SIMC_TOOLS arki-query --data \
	"timerange:GRIB1,2,,;level:GRIB1,105;product:GRIB1,80,2,15 or GRIB1,80,2,16 or GRIB1,80,201,187" \
	grib:$1 >>$2

    # instant - (near)surf
    $SIMC_TOOLS arki-query --data \
	"timerange:GRIB1,0,,;level:GRIB1,1 or GRIB1,4 or GRIB1,102 or GRIB1,105 or GRIB1,111;product:GRIB1,80,2,6 or GRIB1,80,2,81 or GRIB1,80,2,2 or GRIB1,80,2,11 or GRIB1,80,2,17 or GRIB1,80,2,33 or GRIB1,80,2,34 or GRIB1,80,2,51 or GRIB1,80,2,65 or GRIB1,80,2,71 or GRIB1,80,2,73 or GRIB1,80,2,74 or GRIB1,80,2,75 or GRIB1,80,2,85 or GRIB1,80,201,84 or GRIB1,80,201,145 or GRIB1,80,201,146 or GRIB1,80,201,197 or GRIB1,80,201,198" \
	grib:$1 >>$2

    # avg/accum - surf
    $SIMC_TOOLS arki-query --data \
	"timerange:GRIB1,3,, or GRIB1,4,,;level:GRIB1,1;product:GRIB1,80,2,61 or GRIB1,80,2,78 or GRIB1,80,2,79 or GRIB1,80,2,111 or GRIB1,80,2,112 or GRIB1,80,2,121 or GRIB1,80,2,122 or GRIB1,80,2,124 or GRIB1,80,2,125 or GRIB1,80,201,22 or GRIB1,80,201,23 or GRIB1,80,201,102 or GRIB1,80,201,113" \
	grib:$1 >>$2

    # instant - ml
    $SIMC_TOOLS arki-query --data \
	"timerange:GRIB1,0,,;level:GRIB1,110,45,46;product:GRIB1,80,2,11 or GRIB1,80,2,33 or GRIB1,80,2,34" \
	grib:$1 >>$2

    # instant - isobaric
    $SIMC_TOOLS arki-query --data \
	"timerange:GRIB1,0,,;level:GRIB1,100;product:GRIB1,80,2,6 or GRIB1,80,2,11 or GRIB1,80,2,33 or GRIB1,80,2,34 or GRIB1,80,2,39 or GRIB1,80,2,51 or GRIB1,80,2,52" \
	grib:$1 >>$2

    POSTPROC_FORMAT=grib
    log "end make_arki5I"
}

# start exporting all assignments
#set -a
#check_dep
#check_defined
# stop exporting all assignments
#set +a

