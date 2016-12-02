
# @param $1 config file name
arki_getdslist()
{
    sed -n -e '/^\[.*\]$/{s/\[\(.*\)\]/\1/g;p};' $1
}

# @param $1 config file name
# @param $2 key whose value should be printed for every dataset
arki_getdskey()
{
#    sed -n -e '/^ *'$2' *=/{s/^ *'$2' *= *//;p};' $1
    sed -n -e "/^ *$2 *=/{s/^ *$2 *= *//;p};" $1
}

# @fn arki_dailycleanup
# @brief Perform the daily cleanup of a set of arkimet datasets.
# @details This function takes as an argument a collection of arkimet
# datasets (generated with the `arki-mergeconf` command) and performs
# the daily deletion of old data. If the two additional parameters
# `$2` and `$3` are provided, they are applied to every dataset of the
# config file; if the parameters are not provided, the deletion time
# is taken from the `delete age` parameter of each dataset, thus
# datasets without that parameter are not touched.
# @param $1 config file name
# @param $2 days in the past for starting deletion (optional)
# @param $3 days in the past for stopping deletion (optional)
arki_dailycleanup() {
    local dslist
    dslist=`arki_getdskey $1 path`
    for ds in $dslist; do
	if [ -n "$2" -a -n "$3" ]; then
	    s1=$2
	    s2=$3
	else
	    confage=`arki_getdskey $ds/config 'delete age'`
	    s1=0
	    s2=-1
	    if [ -n "$confage" ]; then
		s1=$confage
		s2=$(($confage + 8))
	    fi
	fi
#    arki_dev=`stat -c %D $ARKI_DIR`
	for back in `seq $s1 $s2`; do
            yy=`date -u --date "$back days ago" "+%Y"`
            mm=`date -u --date "$back days ago" "+%m"`
            dd=`date -u --date "$back days ago" "+%d"`

	    rm -f $ds/$yy/$mm-$dd.grib* $ds/$yy/$mm-$dd.bufr
#            if [ "`stat -c %D $file 2>/dev/null`" = "$arki_dev" ]; then
	done
    done
    arki-check --fix --config=$1 # --repack
}

# start exporting all assignments
set -a
# checks
check_dep arkitools
# stop exporting all assignments
set +a
