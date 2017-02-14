cineca_site_init() {
    CINECA_ARCHIVE_PRE=/gpfs/meteo/lm/galileo/auto/archive/PROD
}

getarki_icbc_cineca() {

    local h hinput timerange ana d2h t2h

    for h in `seq $MODEL_START_SLICE $MODEL_FREQ_SLICE $MODEL_STOP_SLICE`; do

	hinput=$(($h+$MODEL_DELTABD_SLICE))
#	timerange="timerange:Timedef,${hinput}h,254"
#	reftime=`getarki_datetime $DATES_SLICE $TIMES_SLICE`
	weekday=`date -u --date $DATES_SLICE +%A`
	origdir=$CINECA_ARCHIVE_PRE/$TIMES_SLICE/$weekday/$PARENTMODEL_ARKI_DS
	if [ ! -f "$origdir/$DATES_SLICE$TIMES_SLICE " ]; then
	    return 1
	fi
# will not work with analysis
	origname=`inputmodel_name $hinput`
	ln -s $origdir/$origname `inputmodel_name $h`

	if [ "$h" -eq "0" ]; then
	    ana=`inputmodel_name a`
	    [ -f "$ana" -o -h "$ana" ] || ln -s `inputmodel_name $h` $ana
	fi
    done

}

# start exporting all assignments
set -a
check_dep cineca_site cosmo_model
# init module
cineca_site_init
# stop exporting all assignments
set +a
