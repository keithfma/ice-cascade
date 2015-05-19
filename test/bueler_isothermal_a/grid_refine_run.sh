#!/bin/bash
#
# SGE scheduler submission script for grid refinement experiment with Test A in
# [1]. This script simply runs all input files named bueler_isothermal_a_in_*.nc
# and writes the results to files named bueler_isothermal_a_out_*.nc.
#
# Keith Ma, May 2015
#
#$ -N bueler_isothermal_a
#$ -P scv
#$ -l h_rt=01:00:00
#$ -j y
#$ -o log.txt

for fin in $(\ls bueler_isothermal_a_in*.nc); do
  fout=${fin/_in_/_out_}
  echo 'Input: ' $fin ' Output: ' $fout
  ./ice-cascade $fin $fout
done
