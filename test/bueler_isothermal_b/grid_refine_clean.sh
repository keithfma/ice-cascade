#!/bin/bash
#
# Cleanup (delete) all files from past grid refinement experiment. Expects one
# command line argument with the path to the directory to clean
# 
# Usage:
#   ./grid_refine_cleanup.sh dir
# 
# Keith Ma, May 2015

if [[ $# -eq 0 ]] ; then 
  echo 'Usage: grid_refine_cleanup.sh dir' 
  exit 1 
fi

rm -f $1/bueler_isothermal_b_in_*.nc
rm -f $1/bueler_isothermal_b_out_*.nc
