#!/bin/bash
#
# Setup build/runtime environment for the BU SCC
#
# Usage:
#   source ./env_bu_scc.sh
#
# Keith Ma, April 2014

module purge
module load netcdf/4.3.0
module load netcdf-fortran/4.4.2
module load fftw/3.3.4
module load ncview/2.1.4
export MPI_COMPILER=gcc

