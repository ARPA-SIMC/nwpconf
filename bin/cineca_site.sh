
import_signal_check_cineca()
{
    local pgdate pgtime weekday
    pgdate=${1:0:8}
    pgtime=${1:8:2}
    weekday=`date -u --date $pgdate +%A`
    [ -f "$CINECA_ARCHIVE_PRE/$pgtime/$weekday/$PARENTMODEL_ARKI_DS/$pgdate$pgtime" ]
}


import_signal_check_cineca_remote()
{
    if [ -n "$CINECA_ARCHIVE_REMOTE" ]; then
	local pgdate pgtime weekday
	pgdate=${1:0:8}
	pgtime=${1:8:2}
	weekday=`date -u --date $pgdate +%A`

# the return value of this ssh command IS the return value of the
# function, else/fi are transparent, but DO NOT put other instructions
# after it
	ssh "$CINECA_ARCHIVE_REMOTE" "[ -f $CINECA_ARCHIVE_REMOTE_PRE/$pgtime/$weekday/$PARENTMODEL_ARKI_DS/$pgdate$pgtime ]"
    else
	false
    fi
}


import_signal_wait_cineca() {
    nwpwait_setup
    local count

    count=0
    while true; do
	count=$(($count + 1))
	import_signal_check_cineca "$1" && return 0 || true
	if [ $(($count%2)) = 0 ]; then
	    import_signal_check_cineca_remote "$1" && return 0 || true
	fi
	nwpwait_wait || return 1
    done
}


getarki_icbc_cineca() {

    local h hinput timerange ana d2h t2h

    if import_signal_check_cineca $DATES_SLICE$TIMES_SLICE; then
# file available locally
	weekday=`date -u --date $DATES_SLICE +%A`
	origdir=$CINECA_ARCHIVE_PRE/$TIMES_SLICE/$weekday/$PARENTMODEL_ARKI_DS

	for h in `seq $MODEL_START_SLICE $MODEL_FREQ_SLICE $MODEL_STOP_SLICE`; do

	    hinput=$(($h+$MODEL_DELTABD_SLICE))
# will not work with analysis
	    origname=`inputmodel_name $hinput`
	    ln -s $origdir/$origname `inputmodel_name $h`

	    if [ "$h" -eq "0" ]; then
		ana=`inputmodel_name a`
		[ -f "$ana" -o -h "$ana" ] || ln -s `inputmodel_name $h` $ana
	    fi
	done
# link constant file with same name
	ln -s $origdir/`inputmodel_name 0`c .

    elif [ -n "$CINECA_ARCHIVE_REMOTE" ]; then
# assume import_signal_check_cineca_remote is true, file available remotely
	weekday=`date -u --date $DATES_SLICE +%A`
	origdir=$CINECA_ARCHIVE_REMOTE_PRE/$TIMES_SLICE/$weekday/$PARENTMODEL_ARKI_DS

	for h in `seq $MODEL_START_SLICE $MODEL_FREQ_SLICE $MODEL_STOP_SLICE`; do

	    hinput=$(($h+$MODEL_DELTABD_SLICE))
# will not work with analysis
	    origname=`inputmodel_name $hinput`
	    scp -p "$CINECA_ARCHIVE_REMOTE:$origdir/$origname" \
		`inputmodel_name $h`

	    if [ "$h" -eq "0" ]; then
		ana=`inputmodel_name a`
		[ -f "$ana" -o -h "$ana" ] || ln -s `inputmodel_name $h` $ana
	    fi
	done
# copy constant file with same name
	scp -p "$CINECA_ARCHIVE_REMOTE:$origdir/`inputmodel_name 0`c" .
    fi

}

# start exporting all assignments
set -a
check_dep cineca_site cosmo_model
# init module
# stop exporting all assignments
set +a
