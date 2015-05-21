#!/bin/bash
#
# SGE scheduler submission script for grid refinement experiment with Test A in
# [1]. This script simply runs all input files named bueler_isothermal_a_in_*.nc
# and writes the results to files named bueler_isothermal_a_out_*.nc. Expects
# one input argument, the directory containing the input (and soon the output)
# files.
#
# Usage: grid_refine_run.sh dir
#
# Keith Ma, May 2015
#
#$ -N bueler_isothermal_a
#$ -P glaciermod 
#$ -l h_rt=12:00:00
#$ -j y
#$ -o log.txt
#$ -V

if [[ $# -eq 0 ]] ; then 
  echo 'Usage: grid_refine_run.sh dir' 
  exit 1 
fi

for fin in $(\ls $1/bueler_isothermal_a_in_*.nc); do
  fout=${fin/_in_/_out_}
  echo 'Input: ' $fin ' Output: ' $fout
  ./ice-cascade $fin $fout
done
