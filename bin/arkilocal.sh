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
## @brief module with functions for working with a local file-based arkimet archive.
## @details This module provides functions for managing a local,
## file-based and serverless arkimet archive. The archive will have
## lower performances and flexibility than a real arkimet archive
## based on an arkimet server, but all the archiving and retrieving
## procedures can work almost transparently. It is useful mainly for
## using the functions designed for continuous assimilation defined in
## the arkiana.sh module. The environment variable `$ARKI_DIR` must be
## exported before sourcing the module, it indicates the root
## directory of the archive, moreover at least one of the
## `$MODEL_ASSIM_GP`, `$MODEL_FCAST_GP`, `$MODEL_INTER_GP`,
## `$MODEL_RADAR_GP` variables indicating the generating processes of
## assimilation, forecast and interpolated from parent model grib
## files should be set.


## @fn arkilocal_setup()
## @brief Creates a local file-based arkimet dataset.
## @details This function creates the directory tree and the basic

## @brief Initialise the environment for using a local file-based arkimet dataset.
## @details This function is implicitly called when the module is
## sourced, it sets the variables `$ARKI_SCAN_METHOD`, `$ARKI_CONF`
## and `$ARKI_DS_*` internally used by the dataset creation function
## and by the archiving and retrieving functions.
arkilocal_setup() {
    local typ gp
# automatically set some variables
    ARKI_SCAN_METHOD=arki-scan
    ARKI_CONF=$ARKI_DIR/config

    for typ in ASSIM FCAST FCRUC INTER RADAR; do
	gp=`eval echo '$'MODEL_${typ}_GP`
	if [ -n "$gp" ]; then
	    eval export ARKI_DS_$typ=$ARKI_DIR/$typ
	fi
    done
}


## @fn arkilocal_create()
## @brief Creates a local file-based arkimet dataset.
## @details This function creates the directory tree and the basic
## configuration files required for working with a local file-based
## and serverless arkimet archive rooted in the directory specified by
## `$ARKI_DIR` (which must contain only a local path
## specification). It creates the datasets `<TYPE>` based on the
## settings of the variables `$MODEL_<TYPE>_GP` inserting a filter
## with the proper generating process in each configuration. It
## creates also the error dataset, required by arkimet, and merges all
## the config files into a single configuration file. It also sets the
## variable `$ARKI_SCAN_METHOD` to `arki-scan` and `$ARKI_CONF` to the
## required value (see putarki.sh::putarki_archive() ) and exports the
## `$ARKI_DS_<TYPE>` variables pointing to the proper dataset.
## @param $1 -c for cleaning the contents of a previous dataset (erases `$ARKI_DIR` directory!)
arkilocal_create() {

    if [ "$1" = "-c" ]; then
	safe_rm_rf $ARKI_DIR/*
    fi
    mkdir -p $ARKI_DIR
# create typical model datasets
    for typ in ASSIM FCAST INTER RADAR; do
	gp=`eval echo '$'MODEL_${typ}_GP`
	filt=`eval echo '$'MODEL_ARKI_TIMERANGE_${typ}`
	if [ -n "$gp" ]; then
	    __arkilocal_create_ds $ARKI_DIR/$typ $typ $gp "$filt"
	fi
    done
# create error dataset, required
    __arkilocal_create_error_ds $ARKI_DIR/error
# merge all confs
    arki-mergeconf $ARKI_DIR/*/ > $ARKI_CONF 2>/dev/null
}

__arkilocal_create_ds() {

    mkdir -p $1
    local delage=
#    if [ -n "$MODEL_SLOW_PAST_H" ]; then
#	delage="delete age = $(($MODEL_SLOW_PAST_H / 24))"
#    fi
    cat > $1/config <<EOF
type = iseg
locking = no
format = grib
name = $2
replace = yes
step = daily
filter = origin:GRIB1,,,$3
index = reftime, timerange, product, level, proddef
unique = reftime, timerange, product, level, area, proddef
$delage
EOF

}

__arkilocal_create_error_ds() {

    mkdir -p $1
    cat > $1/config <<EOF
name = error
locking = no
step = daily
type = error
EOF

}


## @fn arkilocal_drop_from_cache()
## @brief Drops from disk cache the archived files.
## @details This function drops from disk cache the files belonging to
## the local arkimet archive, to avoid filling disk cache with
## long-living files. The content is not touched.
arkilocal_drop_from_cache() {

    find $ARKI_DIR -name \*.grib -type f \
	-exec dd of=\{\} oflag=nocache conv=notrunc,fdatasync count=0 \;

}


# start exporting all assignments
set -a
# checks
check_dep arkilocal
#check_defined ARKI_DIR
# stop exporting all assignments
set +a
