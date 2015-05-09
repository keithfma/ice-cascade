#!/usr/bin/env python
#
# Generate ICE-CASCADE input file for Test B in (1). Grid spacing is an
# (optional) command line argument to facilitate grid refinement experiments.
#
# Usage:
#   ./make_input_bueler_isothermal_a filename nxy
#
# Arguments:
#   filename = name of generated input file (optional)
#   nxy = num grid points in x- and y-dir (optional)
#
# Dependencies:
#   ic_input_tools
#   netcdf4
#
# References:
#
#   (1) Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
#   L.  N. (2005). Exact solutions and verification of numerical models for
#   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
#   doi:10.3189/172756505781829449
#
# Keith Ma, May 2015

import sys
import numpy as np
import ic_input_tools as ic

# defaults
filename = 'bueler_isothermal_b_in.nc' 
nxy = 51 

# parse input arguments
if len(sys.argv) > 1:
  filename = sys.argv[1]
if len(sys.argv) == 3:
  nxy = int(sys.argv[2])
if len(sys.argv) > 3:
  print 'Too many input arguments. Exiting.'
  sys.exit(-1)

# parameters for Bueler test B (see table 3, and equation 10)
alpha = 1./9. # [1] 
beta = 1./18. # [1]
H0 = 3600. # [m]
R0 = 750000. # [m]
t0 = 422.45 # [a]
tf = t0+25.e3 # [a]
g = 9.81 # acceleration of gravity, [m/s2]
rhoi = 910. # ice density, [kg/m3]
A = 10.0e-16 # ice deformation coeff, [Pa-3 a-1]

# general parameters
lxy = 1.1*R0*(tf/t0)**beta # domain dimensions (final radius + 10%)
dt = 1000. # model time step
dtw = dt # model output time step

# coordinate grid
(xy, dxy) = np.linspace(0.0, lxy, num = nxy, retstep = True, dtype = np.float64)

# exact solution (eq 10 with t = t0)
(xx, yy) = np.meshgrid(xy, xy)
rr = np.sqrt(xx**2+yy**2)
mask = np.where(rr/R0 <= 1)
ice_h_soln = np.zeros((nxy,nxy), dtype = np.float64)
ice_h_soln[mask] = H0*(1.-rr[mask]/R0)**(3./7.)

# define parameters and variables
v = ic.null_input(nxy, nxy)
v['descr'] = ('Benchmark case with exact solution (Bueler et al 2005, test B).'
  ' Isothermal, non-sliding, transient ice cap with zero surface ice flux.')
v['nx'] = nxy
v['ny'] = nxy
v['lx'] = lxy
v['ly'] = lxy
v['dx'] = dxy
v['dy'] = dxy
v['rhoi'] = rhoi
v['grav'] = g
v['time_start'] = t0
v['time_finish'] = tf
v['time_step'] = dt
v['time_step_write'] = dtw 
v['ice_name'] = 'mahaffy_isothermal_nonsliding'
v['ice_param'] = [A]
v['ice_soln_name'] = 'bueler_isothermal_b'
v['ice_soln_param'] = [alpha, beta, H0, R0, t0]
v['x'] = xy
v['y'] = xy
v['ice_h'] = ice_h_soln
v['ice_h_soln'] = ice_h_soln
v['write_ice_q_surf'] = 1
v['write_ice_h'] = 1
v['write_ice_h_soln'] = 1 
v['write_ice_uvd'] = 1 

# create input file
ic.create(filename, v)
