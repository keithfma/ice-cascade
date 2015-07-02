#!/usr/bin/env python
#
# Generate ICE-CASCADE input file for Test A in (1). This 'full' variant
#   computes the whole ice cap, as opposed to only the northeast quadrant as in
#   [1]. To simplify calculation of 'dome' errors, the number of gridpoints must
#   be odd so that the origin lies at a grid point. Required input arguments are
#   the number of grid points in both dimensions, the name of the ice flow
#   method (see the parameter supported_flow_methods for possible values) and
#   the name of the generated input file. 
#
# Usage:
#   ./test_bueler_isothermal_a_full nxy flow_method filename 
#
# Arguments:
#   nxy = num grid points in x- and y-dir
#   flow_method = name of ice flow method 
#   filename = name of generated input file
#
# Dependencies:
#   ice_cascade_tools
#   netcdf4
#   numpy
#
# References:
#
#   (1) Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
#   L.  N. (2005). Exact solutions and verification of numerical models for
#   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
#   doi:10.3189/172756505781829449
#
# Keith Ma, June 2015

import sys
import numpy as np
import netCDF4 as nc
import ice_cascade_tools as ict

# define parameters
M0 = 0.3 # surface ice flux, [m_ice/a] 
L = 7.5e5 # ice cap radius, [m]
g = 9.81 # acceleration of gravity, [m/s2]
rhoi = 910. # ice density, [kg/m3]
A = 1.0e-16 # ice deformation coeff, [Pa-3 a-1]
lxy = 1.2*L # domain dimensions (ice cap radius + 20%)
ti = 0. # model start time
tf = 25000. # model end time
dt = 100. # model time step, should be irrelevant
tw = 25000. # output steps
descr = ('Benchmark case with exact solution (Bueler et al 2005, test A). '
  'Isothermal, non-sliding, steady state with fixed margin position and '
  'constant, positive surface ice flux.')
supported_flow_methods = [
    'hindmarsh2_explicit',
    'hindmarsh2_sliding_explicit']

def create(nxy, flow, name):
  '''Create input NETCDF file called "name" for the bueler_isothermal_a test
  case with "nxy" points in each dimension, using the ice flow method "flow"''' 
 
  # check for sane arguments
  if (nxy%2 == 0):
    print('Invalid value for input parameter nxy, must be odd')
    sys.exit()
  if flow not in supported_flow_methods:
    print('ERROR: flow method not supported (' + flow + ')')
    sys.exit()

  # coordinate grid
  dxy = 2.*lxy/float(nxy-1)
  nhalf = (nxy-1)/2
  xy = dxy*np.arange(-nhalf, nhalf+1, 1, dtype = np.float64) 
  
  # exact solution
  (xx, yy) = np.meshgrid(xy, xy)
  rr = np.sqrt(xx**2+yy**2)
  gamma = 2.0/5.0*A*(rhoi*g)**3.0
  mask = np.where(rr <= L)
  ice_h_soln = np.zeros((nxy,nxy), dtype = np.float64)
  ice_h_soln[mask] = (4.0*M0/gamma)**(1.0/8.0)*(L**(4.0/3.0)-rr[mask]**(4.0/3.0))**(3.0/8.0)
  
  # create and open new input file
  file = ict.new_input(name, nxy, nxy)
  
  # write data common to all ice flow methods
  file.descr = descr
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
  file.climate_name = 'bueler_isothermal_a'
  file.climate_param__var = [M0, L]
  file.ice_bc_name__nesw = 'no_ice,no_ice,no_flux,no_flux'
  file.ice_soln_name = 'bueler_isothermal_a'
  file.ice_soln_param__var = [M0, L, A]
  file.write_ice_q_surf = 1
  file.write_ice_h = 1
  file.write_ice_h_soln = 1 
  file.variables['x'][:] = xy
  file.variables['y'][:] = xy
  file.variables['topo'][:,:] = 0.
  file.variables['ice_h'][:,:] = ice_h_soln
  file.variables['ice_h_soln'][:,:] = ice_h_soln
  
  # write data specific to each flow method
  if flow == 'hindmarsh2_explicit':
    file.ice_name = 'hindmarsh2_explicit'
    file.ice_param__var = [A]

  elif flow == 'hindmarsh2_sliding_explicit':
    file.ice_name = 'hindmarsh2_sliding_explicit'
    file.ice_param__var = []
    file.variables['ice_a_defm'][:,:] = A
    file.variables['ice_a_slid'][:,:] = 0.
  
  # finalize
  file.close()

if __name__ == '__main__':

  (n, flow, name) = ict.test_read_args()
  create(n, flow, name)
