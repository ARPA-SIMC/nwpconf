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
    export NWPCONF=production/dailymodelrun/forecast
    # source the main libary file
    . $NWPCONFBINDIR/nwpconf.sh


The conf/ directory in the source tree contains a sample configuration
tree, run the `testenv.sh` script from within that directory to try
it.

The documentation can be built with the help of
[Doxygen](http://www.stack.nl/~dimitri/doxygen/index.html) and the
additional `doxygen-bash.sed` script for extracting documentation from
shell scripts, available from the [bash-doxygen
project](https://github.com/Anvil/bash-doxygen).
