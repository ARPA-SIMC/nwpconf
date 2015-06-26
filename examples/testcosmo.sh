!/bin/bash
# sample script for testing the basic features of nwpconf package

# setup common to user scripts
# basic variables
export NWPCONFDIR=$PWD/conf
export NWPCONFBINDIR=$PWD/../bin
export PROFILE=production
export PROCESS=dailymodelrun
export PHASE=forecast
# source the main library module
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
. $NWPCONFBINDIR/sms_tools.sh
. $NWPCONFBINDIR/nwptime.sh
. $NWPCONFBINDIR/getarki.sh
. $NWPCONFBINDIR/cosmo_model.sh
# end of setup

nwpbctimeloop_init
while nwpbctimeloop_loop; do
    echo $D1 $T1 $D2 $T2 $MODEL_START $MODEL_STOP
    getarki_icbc
done
