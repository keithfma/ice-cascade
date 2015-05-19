#!/usr/bin/env python
#
# Prepare input files for grid refinement experiment for Test A in [1]. Depends
# on the make_input.py script to generate input files.
#
# References:
#
#   (1) Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
#   L.  N. (2005). Exact solutions and verification of numerical models for
#   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
#   doi:10.3189/172756505781829449
#
# Keith Ma, May 2015

import make_input

# define grid sizes
nxy = [16, 32, 64]
#nxy = [16, 32, 64, 128, 256]

# figure out padding
d = 1
while (max(nxy) > 10**d):
  print 10**d
  d = d+1

# generate files
for n in nxy:
  filename = 'bueler_isothermal_a_in_'+str(n).zfill(d)+'.nc'
  print filename
  make_input.main(filename, n)
  


