# NWPconf

NWPconf is a library of bash shell functions that simplify the task of
running Numerical Weather Prediction or --more generally-- Environmental
models on a regular daily basis, taking into account the basic
operations related to date/time computations, defining test
configurations, creating configuration files from templates and
parallel process managing.

The library consists in a main bash/ksh shell module, _nwpconf.sh_,
and a set of optional modules. These modules can be used within a
regular shell script in bash or ksh shells as well as within a batch
job submitted to a scheduling system, provided that the shell chosen
for the batch job is bash or ksh.

In order to use the library, the user shell script must export some
valuable environmental variables (see the documentation of each module
for the correspoding variables and explanation), and _source_ the
desired module files, in the following way:

    # Export main variables (customize)
    export NWPCONFDIR=$HOME/conf
    export NWPCONFBINDIR=$HOME/nwpconf/bin
    export PROFILE=production
    export PROCESS=dailymodelrun
    export PHASE=forecast
    # source the main libary file
    . $NWPCONFBINDIR/nwpconf.sh



