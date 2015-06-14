#!/usr/bin/env python
#
# Generate ICE-CASCADE input file for test E in [1]. Grid spacing is an
# (optional) command line argument to facilitate grid refinement experiments. To
# simplify calculation of 'dome' errors, the number of gridpoints must be odd so
# that the origin lies at a grid point.
#
# Usage:
#   ./make_input_bueler_isothermal_e filename nxy
#
# Arguments:
#   filename = name of generated input file (optional)
#   nxy = num grid points in x- and y-dir (optional)
#
# Dependencies:
#   ice_cascade_tools
#   netcdf4
#   numpy
#
# References:
#
#   [1] Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
#   L.  N. (2005). Exact solutions and verification of numerical models for
#   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
#   doi:10.3189/172756505781829449
#
# Keith Ma, June 2015

import sys
import numpy as np
import netCDF4 as nc
import ice_cascade_tools as ict

# parameters for Bueler test E
M0 = 0.3 # [m/a]
L = 750000. # [m]
mumax = 2.5e-11*31556926. # [m/Pa/a]
r1 = 200000. # [m]
r2 = 700000. # [m]
theta1 = 10 # [degrees]
theta2 = 40 # [degrees]

# general parameters

# main function
def main(filename, nxy):

  # coordinate grids

  # sliding coefficient

  # exact solution

  # create and open new input file
  file = ict.new_input(filename, nxy, nxy)
  
  # define parameters and variables 

  
# run as script
if __name__ == '__main__':

  # defaults
  nxy = 51 
  filename = 'bueler_isothermal_d_in_'+str(nxy)+'.nc' 
  
  # parse input arguments
  if len(sys.argv) == 2:
    filename = sys.argv[1]
  if len(sys.argv) == 3:
    filename = sys.argv[1]
    nxy = int(sys.argv[2])
  if len(sys.argv) > 3:
    print 'Too many input arguments. Exiting.'
    sys.exit(-1)

  # make it
  main(filename, nxy)
