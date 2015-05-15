#!/usr/bin/env python
#
# Generate ICE-CASCADE input file for Test A in (1). Grid spacing is an
# (optional) command line argument to facilitate grid refinement experiments.
#
# Usage:
#   ./make_input_bueler_isothermal_a filename nxy
#
# Arguments:
#   stub = name of generated input files (optional)
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
# Keith Ma, April 2015

import sys
import numpy as np
import ic_input_tools as ic

# defaults
filename = 'bueler_isothermal_a_in.nc' 
nxy = 51 

# parse input arguments
if len(sys.argv) > 1:
  filename = sys.argv[1]
if len(sys.argv) == 3:
  nxy = int(sys.argv[2])
if len(sys.argv) > 3:
  print 'Too many input arguments. Exiting.'
  sys.exit(-1)

# parameters for Bueler test A 
M0 = 0.3 # surface ice flux, [m_ice/a] 
L = 7.5e5 # ice cap radius, [m]
g = 9.81 # acceleration of gravity, [m/s2]
rhoi = 910. # ice density, [kg/m3]
A = 10.0e-16 # ice deformation coeff, [Pa-3 a-1]

# general parameters
lxy = 1.1*L # domain dimensions, 10% larger than icecap
ti = 0. # model start time
tf = 100. # model end time
dt = 100. # model time step
dtw = dt # model output time step

#   coordinate grid
(xy, dxy) = np.linspace(0.0, lxy, num = nxy, retstep = True, dtype = np.float64)

#   exact solution
(xx, yy) = np.meshgrid(xy, xy)
rr = np.sqrt(xx**2+yy**2)
gamma = 2.0/5.0*A*(rhoi*g)**3.0
mask = np.where(rr <= L)
ice_h_soln = np.zeros((nxy,nxy), dtype = np.float64)
ice_h_soln[mask] = (4.0*M0/gamma)**(1.0/8.0)*(L**(4.0/3.0)-rr[mask]**(4.0/3.0))**(3.0/8.0)

# define parameters and variables 
v = ic.null_input(nxy, nxy)
v['descr'] = ('Benchmark case with exact solution (Bueler et al 2005, test A).'
  ' Isothermal, non-sliding, steady state with fixed margin position and '
  'constant, positive surface ice flux.')
v['nx'] = nxy
v['ny'] = nxy
v['lx'] = lxy
v['ly'] = lxy
v['dx'] = dxy
v['dy'] = dxy
v['rhoi'] = rhoi
v['grav'] = g
v['time_start'] = ti
v['time_finish'] = tf
v['time_step'] = dt
v['time_step_write'] = dtw 
v['climate_name'] = 'bueler_isothermal_a'
v['climate_param'] = [M0, L]
v['ice_name'] = 'hindmarsh2_explicit'
v['ice_param'] = [A]
v['ice_bc_name__nesw'] = 'mirror,mirror,mirror,mirror'
v['ice_soln_name'] = 'none'
v['ice_soln_param'] = [M0, L, A]
v['x'] = xy
v['y'] = xy
#v['ice_h'] = ice_h_soln
v['ice_h_soln'] = ice_h_soln
v['write_ice_q_surf'] = 1
v['write_ice_h'] = 1
v['write_ice_h_soln'] = 1 
v['write_ice_uvd'] = 1 

# create input file
ic.create(filename, v)

