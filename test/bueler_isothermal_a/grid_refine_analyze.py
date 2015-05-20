#!/usr/bin/env python
#
# Analyze results from grid refinement experiment for Test A in [1]. Expects one
# command line argument, the directory containing the output files.
#
# Usage: grid_refine_analyze.py dir
#
# References:
#
#   (1) Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
#   L.  N. (2005). Exact solutions and verification of numerical models for
#   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
#   doi:10.3189/172756505781829449
#
# Keith Ma, May 2015

import glob
import sys
import numpy as np
import netCDF4 as nc
import matplotlib.pyplot as plt

# parse input arguments
if len(sys.argv) != 2:
  print 'Usage: grid_refine_analyze.py dir'
  sys.exit()
else: 
  dir = sys.argv[1]

# init empty lists
nx = [] # num grid points 
dx = [] # grid spacing, m
h = [] # final numerical ice thickness
h_soln = [] # final exact ice thickness
h_err = [] # error (exact = numerical)
h_err_abs = [] # abs(error)
h_err_abs_max = [] # max of abs(error)
h_err_abs_mean = [] # mean of abs(error)
h_err_abs_std = [] # standard deviation of abs(error)
h_err_dome = [] # error at ice cap center

# loop over output files
files = glob.glob(dir+'/bueler_isothermal_a_out_*.nc')
for i in range(len(files)):

  # read data
  file = nc.Dataset(files[i], mode = 'r')
  nx.append(file.nx__1)
  dx.append(file.dx__m)
  h.append(file.variables['ice_h'][0,:,:])
  h_soln.append(file.variables['ice_h_soln'][0,:,:])
  file.close()

  # compute error, map and scalar statistics
  mask = np.logical_or(np.greater(h[i], 0.), np.greater(h_soln[i], 0.)) 
  h_err.append(h_soln[i]-h[i])
  h_err_abs.append(abs(h_err[i]))
  h_err_abs_max.append(h_err_abs[i].max())
  h_err_abs_mean.append(np.mean(h_err_abs[i][mask]))
  h_err_abs_std.append(np.std(h_err_abs[i][mask]))
  h_err_dome.append(h_err[i][0,0])
  
  # debug
  print nx[i], dx[i], h_err_abs_max[i], h_err_abs_mean[i], h_err_abs_std[i], h_err_dome[i]


# fit power law function to scalar results

# plot scalar results

# tabulate scalar results

# plot difference maps

# generate pdf

