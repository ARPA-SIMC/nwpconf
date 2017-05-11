cineca_site_init() {
    CINECA_ARCHIVE_PRE=/gpfs/meteo/lm/galileo/auto/archive/PROD
}


import_signal_check_cineca()
{
    local pgdate pgtime weekday
    pgdate=${1:0:8}
    pgtime=${1:8:2}
    weekday=`date -u --date $pgdate +%A`
    [ -f "$CINECA_ARCHIVE_PRE/$pgtime/$weekday/$PARENTMODEL_ARKI_DS/$pgdate$pgtime" ]
}


import_signal_wait_cineca() {
    nwpwait_setup

    while true; do
	import_signal_check_cineca "$1" && return 0 || true
	nwpwait_wait || return 1
    done
}


getarki_icbc_cineca() {

    local h hinput timerange ana d2h t2h

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

}

# start exporting all assignments
set -a
check_dep cineca_site cosmo_model
# init module
cineca_site_init
# stop exporting all assignments
set +a
