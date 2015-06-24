
# configuration setup and output templating functions

_confdirlist_add() {
    if [ -d $1 ]; then
	confdirlist="$confdirlist $1"
    fi
}

# improve, avoid adding /conf
conf_init() {
    confdirlist=''
    _confdirlist_add $CFDIR
    for prof in $PROFILE; do
	_confdirlist_add $CFDIR/$prof
	if [ -n "$PROCESS" ]; then
	    _confdirlist_add $CFDIR/$prof/$PROCESS
	    if [ -n "$PHASE" ]; then
		_confdirlist_add $CFDIR/$prof/$PROCESS/$PHASE
	    fi
	fi
    done
}

conf_source() {
    for dir in $confdirlist; do
	if [ -f "$dir/conf.sh" ]; then
	    source "$dir/conf.sh"
	fi
    done
}

conf_getfile() {
    local conffile=''
    for dir in $confdirlist; do
	if [ -f "$dir/$1" ]; then
	    conffile="$dir/$1"
	fi
    done
    echo $conffile
}

conf_template() {
    for file in $*; do
	template=`conf_getfile $file.in`
	if [ -n $template ]; then
	    $CFBINDIR/ac_templater.py $template > $file
	fi
    done
}

parcomp_computetopo() {
    if [ "$1" -ge 128 ]; then
	NPX=8
	NPY=16
    elif [ "$1" -ge 96 ]; then
	NPX=6
	NPY=16
    elif [ "$1" -ge 64 ]; then
	NPX=4
	NPY=16
    elif [ "$1" -ge 60 ]; then
	NPX=6
	NPY=10
    elif [ "$1" -ge 56 ]; then
	NPX=4
	NPY=14
    elif [ "$1" -ge 54 ]; then
	NPX=6
	NPY=9
    elif [ "$1" -ge 50 ]; then
	NPX=5
	NPY=10
    elif [ "$1" -ge 48 ]; then
	NPX=4
	NPY=12
    elif [ "$1" -ge 44 ]; then
	NPX=4
	NPY=11
    elif [ "$1" -ge 42 ]; then
	NPX=3
	NPY=14
    elif [ "$1" -ge 40 ]; then
	NPX=4
	NPY=10
    elif [ "$1" -ge 36 ]; then
	NPX=4
	NPY=9
    elif [ "$1" -ge 32 ]; then
	NPX=4
	NPY=8
    elif [ "$1" -ge 30 ]; then
	NPX=3
	NPY=10
    elif [ "$1" -ge 28 ]; then
	NPX=4
	NPY=7
    elif [ "$1" -ge 24 ]; then
	NPX=3
	NPY=8
    elif [ "$1" -ge 21 ]; then
	NPX=3
	NPY=7
    elif [ "$1" -ge 20 ]; then
	NPX=4
	NPY=5
    elif [ "$1" -ge 18 ]; then
	NPX=3
	NPY=6
    elif [ "$1" -ge 16 ]; then
	NPX=4
	NPY=4
    elif [ "$1" -ge 12 ]; then
	NPX=3
	NPY=4
    elif [ "$1" -ge 9 ]; then
	NPX=3
	NPY=3
    elif [ "$1" -ge 8 ]; then
	NPX=2
	NPY=4
    elif [ "$1" -ge 4 ]; then
	NPX=2
	NPY=2
    elif [ "$1" -ge 2 ]; then
	NPX=1
	NPY=2
    else # troppo pochi
	NPX=1
	NPY=1
    fi
}

parcomp_init() {
# check existence of $PBS_NODEFILE?
# -a "$USEQSUB" = "YES"
#    if [ "$PARALLEL_TASK" = "YES" ]; then
	if [ -n "$SLURM_NTASKS" ]; then
	    parcomp_computetopo $SLURM_NTASKS
	elif [ -n "$PBS_NODEFILE" -a -f "$PBS_NODEFILE" ]; then
	    parcomp_computetopo `wc -l "$PBS_NODEFILE"`
	else
	    parcomp_computetopo 1
	fi
	MPIHOST=$PBS_NODEFILE
	NPIO=0
	NP=$(($NPX*$NPY+$NPIO))
#    fi
}

# run a parallel MPI process starting the required number of processes
# the arguments are additional options to mpirun and the executable
# name
parcomp_mpirun() {
# adapt to mpi/queuing system used
    mpirun -np $NP $*
# mpirun -np $NP --hostfile $MPIHOST $LMBIN
}

# functions for returning on stdout date or time of day (hours) or
# both incremented or decremented by the requested amount of hours
# arguments are date (YYYYMMDD), time (HH) and increment in hours

date_add() {
    $DATECOM --date "$1 $2:00 `signedhour_to_date $3`" '+%Y%m%d'
}

time_add() {
    $DATECOM --date "$1 $2:00 `signedhour_to_date $3`" '+%H'
}

datetime_add() {
    $DATECOM --date "$1 $2:00 `signedhour_to_date $3`" '+%Y%m%d%H'
}

date_sub() {
    $DATECOM --date "$1 $2:00 `minus_signedhour_to_date $3`" '+%Y%m%d'
}

time_sub() {
    $DATECOM --date "$1 $2:00 `minus_signedhour_to_date $3`" '+%H'
}

datetime_sub() {
    $DATECOM --date "$1 $2:00 `minus_signedhour_to_date $3`" '+%Y%m%d%H'
}

signedhour_to_date() {
    if [ "$1" -lt 0 ]; then
	echo "$((-$1)) hours ago"
    else
	echo "$1 hours"
    fi
}

minus_signedhour_to_date() {
    if [ "$1" -lt 0 ]; then
	echo "$((-$1)) hours"
    else
	echo "$1 hours ago"
    fi
}

# The date and time as requested by reftime arki-query key
datetime_arki() {
    $DATECOM --date "$1 $2:00" '+%Y-%m-%d %H:00'
}

# Delta time to be used in COSMO grib file names, input forecast time
# in h, output ddhh0000
timedelta_cosmo() {
    local d=0
    local h=$1
    while [ "$h" -ge 24 ]; do
	d=$(($d+1))
	h=$(($h-24))
    done
    printf "%02d%02d0000\n" $d $h
    
}

# max and min of the two numerical arguments
max() {
    if [ "$1" -gt "$2" ]; then
	echo $1
    else
	echo $2
    fi
}

min() {
    if [ "$1" -lt "$2" ]; then
	echo $1
    else
	echo $2
    fi
}


# logsim wait function
wait_logsim() {

    while true; do
# \\pset format unaligned \\\\
	if [ -n "$1" ]; then
	    n=`echo "SELECT count(*) FROM imports i, entities e WHERE i.entity_id = e.id AND message = '$1' AND reftime = timestamp with time zone '$2:00+00' AND name = '$3';" \
		| psql -h log.metarpa -d simclogdb -U simclog -A -F ',' -n -q -t`
	else
	    n=`echo "SELECT count(*) FROM imports i, entities e WHERE i.entity_id = e.id AND reftime = timestamp with time zone '$2:00+00' AND name = '$3';" \
		| psql -h log.metarpa -d simclogdb -U simclog -A -F ',' -n -q -t`
	fi
	if [ "$n" -ge 1 ]; then
	    return 0
	fi
	sleep 60
    done
# password in ~/.pgpass
}


int2lm_once_init() {
# start of COSMO run
    D1=`date_sub $DATE $TIME $COSMO_BACK`
    T1=`time_sub $DATE $TIME $COSMO_BACK`
    D2=$D1
    T2=$T1
# COSMO_DELTABD is difference (hours) between $DATE$TIME (end of
# assimilation window / start of forecast) and start of last available
# input forecast providing BC (for BCANA=N)
    if [ "$COSMO_BCANA" = "Y" ]; then
	export COSMO_FREQ_INPUT=$COSMO_FREQANA_INPUT
	COSMO_DELTABD=0
    else
	export COSMO_FREQ_INPUT=$COSMO_FREQFC_INPUT
    fi
    DELTABD=0
    # DELTABD=$(($COSMO_DELTABD-$COSMO_BACK))
    # while [ $DELTABD -lt 0 ]; do
    # 	DELTABD=$(($DELTABD+$COSMO_FREQANA_INPUT))
    # done
    # DELTABDLOCAL=$DELTABD
}

int2lm_loop_init() {
    int2lm_once_init
    COSMO_FULL_STOP=$COSMO_STOP
    COSMO_STOP=0
# DELTABD is difference between start of assimilation and start of
# input forecast suitable for providing BC
    if [ "$COSMO_BCANA" != "Y" ]; then
	DELTABD=$(($COSMO_DELTABD-$COSMO_BACK))
	while [ $DELTABD -lt 0 ]; do
	    DELTABD=$(($DELTABD+$COSMO_FREQINI_INPUT))
	done
	D3=`date_sub $DATE $TIME $COSMO_DELTABD`
	T3=`time_sub $DATE $TIME $COSMO_DELTABD`
    fi
    DELTABDLOCAL=$DELTABD
}


int2lm_loop() {
    [ $COSMO_STOP -lt $COSMO_FULL_STOP ] || return 1
    DELTABD=$DELTABDLOCAL
    D2=`date_sub $D1 $T1 $DELTABD`
    T2=`time_sub $D1 $T1 $DELTABD`

    if [ "$1" = "-f" ]; then
	COSMO_INT2LMINDIR=`eval echo \\$${COSMO_INPUT}_DIR`/$D2$T2
    fi
    COSMO_START=`max 0 $((-$DELTABD))`
    if [ "$COSMO_BCANA" = "Y" ]; then
	COSMO_STOP=$COSMO_START
# prepare for next loop, update DELTABDLOCAL so that DELTABD can be
# used outside
	DELTABDLOCAL=$(($DELTABD-$COSMO_FREQANA_INPUT))
    else
# test whether this is the last possible loop
	DT=`date_add $D2 $T2 $COSMO_FREQINI_INPUT`
	TT=`time_add $D2 $T2 $COSMO_FREQINI_INPUT`
	if [ $DT$TT -gt $D3$T3 ]; then
	    COSMO_STOP=$COSMO_FULL_STOP
	else
	    COSMO_STOP=`min $COSMO_FULL_STOP $(($COSMO_FREQINI_INPUT-$DELTABD-$COSMO_FREQFC_INPUT))`
	fi
# prepare for next loop, update DELTABDLOCAL so that DELTABD can be
# used outside
	DELTABDLOCAL=$(($DELTABD-$COSMO_FREQINI_INPUT))
    fi

    if [ $COSMO_START -eq 0 ]; then
	COSMO_LANA=.TRUE.
    else
	COSMO_LANA=.FALSE.
    fi

    return 0

}


cosmo_init() {
    int2lm_once_init
}


init_sms_meter() {

    SMSMETER=$1
    if [ -n "$2" ]; then
        SMSMETER_COUNT=$2
    else
        SMSMETER_COUNT=0
    fi
    $CFBINDIR/timeout.sh 10 smsmeter $SMSMETER $SMSMETER_COUNT || true
 
}


increment_sms_meter() {

    if [ -n "$SMSMETER" ]; then
        [ -z "$SMSMETER_COUNT" ] && SMSMETER_COUNT=0
        SMSMETER_COUNT=$(($SMSMETER_COUNT + 1))
        $CFBINDIR/timeout.sh 10 smsmeter $SMSMETER $SMSMETER_COUNT || true
    fi

}


# start exporting all assignments
set -a
# create confdirlist
conf_init
# import configuration
conf_source
# compute processor topology
parcomp_init
# stop exporting all assignments
set +a

