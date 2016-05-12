# NWPconf

NWPconf is a library of bash shell functions that simplify the task of
running Numerical Weather Prediction or --more generally--
Environmental models on a regular daily basis, taking into account the
basic operations related to date/time computations, defining test
configurations, creating configuration files from templates, preparing
input data and managing parallel processes.

The library consists of a main bash shell module, _nwpconf.sh_, and a
set of optional modules. These modules can be used within a regular
shell script in bash shell as well as within a batch job submitted to
a scheduling system, provided that the shell chosen for the batch job
is bash.

In order to use the library, the user shell script must export some
valuable environmental variables (see the documentation of each module
for the correspoding variables and explanation), and _source_ the
desired module files, in the following way:

    # Export main variables (customize)
    export NWPCONFDIR=$HOME/conf
    export NWPCONFBINDIR=$HOME/nwpconf/bin
    export NWPCONF=production/dailymodelrun/forecast
    # source the main library file
    . $NWPCONFBINDIR/nwpconf.sh
    # source other optional modules
    . $NWPCONFBINDIR/nwptime.sh


The modules contain mainly shell functions, but some short
initialisation code may be executed by some modules when being
sourced. The functions have been written avoiding the generation of
nonzero return codes for non-fatal error situations, so it is safe and
even suggested to run with the shell setting

    set -e

so that the execution will stop in case of serious errors.

The examples/conf/ directory in the source tree contains a sample
configuration tree, run the `testenv.sh` script from within the
examples/ directory to try it.

All the modules have been written for and tested with the bash shell;
most of the functions may work also with the traditional ksh shell,
but some will for sure fail due to the use of some bash-specific
constructs.

The documentation can be built with the help of
[Doxygen](http://www.stack.nl/~dimitri/doxygen/index.html) and the
additional `doxygen-bash.sed` script for extracting documentation from
shell scripts, available from the [bash-doxygen
project](https://github.com/Anvil/bash-doxygen).
