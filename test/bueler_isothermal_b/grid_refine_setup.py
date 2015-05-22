#!/usr/bin/env python
#
# Prepare input files for grid refinement experiment for Test B in [1]. Depends
# on the make_input.py script to generate input files. Expects one input
# argument with the directory to write the files to.
#
# Usage ./grid_refine_setup.py dir
#
# References:
#   [1] Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
#   L.  N. (2005). Exact solutions and verification of numerical models for
#   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
#   doi:10.3189/172756505781829449
#
# Keith Ma, May 2015

import sys
import os
import make_input

# parse input arguments
if len(sys.argv) != 2:
  print 'Usage: grid_refine_setup.py dir'
  sys.exit()
else: 
  dir = sys.argv[1]
  if not os.path.exists(dir):
    os.makedirs(dir)

# define grid sizes
nxy = [30, 60, 120, 240] 

# figure out padding
d = 1
while (max(nxy) > 10**d):
  d = d+1

# generate files
for n in nxy:
  filename = dir+'/bueler_isothermal_b_in_'+str(n).zfill(d)+'.nc'
  print filename
  make_input.main(filename, n)
