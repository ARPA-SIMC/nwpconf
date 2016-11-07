
# @param $1 config file name
arki_getdslist()
{
    sed -n -e '/^\[.*\]$/{s/\[\(.*\)\]/\1/g;p};' $1
}

# @param $1 config file name
# @param $2 key whose value should be printed for every dataset
arki_getdskey()
{
    sed -n -e '/^ *'$2' *=/{s/^ *'$2' *= *//;p};' $1
}

# config 8 12
# @param $1 config file name
# @param $2 days in the past for starting deletion
# @param $3 days in the past for stopping deletion
arki_dailycleanup() {
    local dslist
    dslist=`arki_getdskey $1 path`
    
#    arki_dev=`stat -c %D $ARKI_DIR`
    for back in `seq $2 $3`; do
        yy=`date -u --date "$back days ago" "+%Y"`
        mm=`date -u --date "$back days ago" "+%m"`
        dd=`date -u --date "$back days ago" "+%d"`
        echo "cleaning $yy/$mm-$dd.grib1"
	for ds in $dslist; do
	    rm -f $ds/$yy/$mm-$dd.grib* $ds/$yy/$mm-$dd.bufr
	done
#        for file in $1/$yy/$mm-$dd.grib* $1/$yy/$mm-$dd.bufr; do
#            if [ "`stat -c %D $file 2>/dev/null`" = "$arki_dev" ]; then
#                rm -f $file
#            fi
#        done
    done
#    log "start arki-check"
    arki-check --fix --config=$1 # --repack
#    log "done arki-check"
}

# start exporting all assignments
set -a
# checks
check_dep arkitools
# stop exporting all assignments
set +
