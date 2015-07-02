#!/bin/bash
#
# Script to setup the bash shell environment properly to run ice-cascade and the
# associated utility scripts. 
#
# Usage:
#   source setup_environ.sh system_name
#
# Keith Ma, July 2015

if [ "$1" == "keithma_bu_scc" ]
then
  module purge
  module load anaconda/2.2.0
  source activate py27
  export PYTHONPATH=$(pwd):$(pwd)/../test:$PYTHONPATH

else 
  echo "system_name not recognized, no changes were made to the environment"

fi


