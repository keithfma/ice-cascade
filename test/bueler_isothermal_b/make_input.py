#!/usr/bin/env python
#
# Generate ICE-CASCADE input file for Test B in (1). Grid spacing is an
# (optional) command line argument to facilitate grid refinement experiments.
#
# Usage:
#   ./make_input_bueler_isothermal_b filename nxy
#
# Arguments:
#   filename = name of generated input file (optional)
#   nxy = num grid points in x- and y-dir (optional)
#
# Dependencies:
#   ice_cascade_tools
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
import netCDF4 as nc
import ice_cascade_tools as ict

# parameters for Bueler test B (see table 3, and equation 10)
alpha = 1./9. # [1] 
beta = 1./18. # [1]
H0 = 3600. # [m]
R0 = 750000. # [m]
t0 = 422.45 # [a]
g = 9.81 # acceleration of gravity, [m/s2]
rhoi = 910. # ice density, [kg/m3]
A = 1.0e-16 # ice deformation coeff, [Pa-3 a-1]

# general parameters
ti = t0
tf = t0+25.e3 # [a]
dt = 1000. # model time step
tw = 25000. # output steps
lxy = 1.1*R0*(tf/t0)**beta # domain dimensions (final radius + 10%)

# main function
def main(filename, nxy):
  
  # coordinate grid
  (xy, dxy) = np.linspace(0.0, lxy, num = nxy, retstep = True, dtype = np.float64)
  
  # exact solution (eq 10 with t = t0)
  (xx, yy) = np.meshgrid(xy, xy)
  rr = np.sqrt(xx**2+yy**2)
  mask = np.where(rr/R0 <= 1)
  ice_h_soln = np.zeros((nxy,nxy), dtype = np.float64)
  ice_h_soln[mask] = H0*(1.-rr[mask]/R0)**(3./7.)
  
  # create and open new input file
  file = ict.new_input(filename, nxy, nxy)
  
  # define parameters and variables 
  file.descr = '''Benchmark case with exact solution (Bueler et al 2005, test
    B). Isothermal, non-sliding, transient ice cap with zero surface ice
    flux.'''
  file.nx__1 = nxy
  file.ny__1 = nxy
  file.lx__m = lxy
  file.ly__m = lxy
  file.dx__m = dxy
  file.dy__m = dxy
  file.rhoi__kg_m3 = rhoi
  file.grav__m_s2 = g
  file.time_start__a = ti
  file.time_finish__a = tf
  file.time_step__a = dt
  file.time_write__a = tw 
  file.climate_name = 'constant_ice'
  file.climate_param__var = [0.]
  file.ice_name = 'hindmarsh2_explicit'
  file.ice_param__var = [A]
  file.ice_bc_name__nesw = 'no_ice,no_ice,mirror,mirror'
  file.ice_soln_name = 'bueler_isothermal_b'
  file.ice_soln_param__var = [alpha, beta, H0, R0, t0]
  file.write_ice_h = 1
  file.write_ice_h_dot = 1
  file.write_ice_h_soln = 1 
  file.variables['x'][:] = xy
  file.variables['y'][:] = xy
  file.variables['ice_q_surf'][:,:] = np.zeros((nxy, nxy), dtype = np.float64)
  file.variables['ice_h'][:,:] = ice_h_soln
  file.variables['ice_h_soln'][:,:] = ice_h_soln
  
  # finalize
  file.close()

if __name__ == '__main__':

  # defaults
  nxy = 51 
  filename = 'bueler_isothermal_b_in_'+str(nxy)+'.nc' 
  
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
