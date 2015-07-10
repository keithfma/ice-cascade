#!/usr/bin/env python
#
# Generate ICE-CASCADE input file for test D in [1]. Required input arguments
#   are the number of grid points in both dimensions, the name of the ice flow
#   method (see the parameter supported_flow_methods for possible values) and
#   the name of the generated input file. 
#
# Usage:
#   ./test_bueler_isothermal_d nxy flow_method filename 
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
H0 = 3600. # [m]
L = 750000. # [m]
Cp = 200. # [m]
Tp = 5000. # [a]
q0 = -0.1 # [m/a]
g = 9.81 # acceleration of gravity, [m/s2]
rhoi = 910. # ice density, [kg/m3]
A = 1.0e-16 # ice deformation coeff, [Pa-3 a-1]
ti = 0. # [a]
tf = ti+5.*Tp # [a]
dt = 1. # model time step
tw = np.linspace(ti, tf, 10*5+1) # output steps
lxy = 1.1*L # domain dimensions (final radius + 10%)
descr = ('Benchmark case with exact solution (Bueler et al 2005, test D).'
  'Isothermal, non-sliding, transient ice cap with oscillating surface ice '
  'flux')
supported_flow_methods = [
    'hindmarsh2_explicit',
    'hindmarsh2_sliding1_explicit']


def create(nxy, flow, name):
  '''Create input NETCDF file called "name" for the bueler_isothermal_a test
  case with "nxy" points in each dimension, using the ice flow method "flow"''' 

  # check for sane arguments
  if flow not in supported_flow_methods:
    print('ERROR: flow method not supported (' + flow + ')')
    sys.exit()

  # coordinate grid
  (xy, dxy) = np.linspace(0.0, lxy, num = nxy, retstep = True, dtype = np.float64)
  (xx, yy) = np.meshgrid(xy, xy)
  rr = np.sqrt(xx**2+yy**2)
  
  # exact solution (eq. 21 and eq. 23 with t = ti)
  Hs = np.zeros((nxy,nxy), dtype = np.float64)
  mask = np.where(rr < L)
  s = rr[mask]/L
  Hs[mask] = H0/(2./3.)**(3./8.)*((4./3.)*s-1./3.+(1.-s)**(4./3.)-s**(4./3.))**(3./8.)

  P = np.zeros((nxy,nxy), dtype = np.float64)
  mask = np.where(np.logical_and(rr > 0.3*L, rr < 0.9*L))
  r = rr[mask]
  P[mask] = Cp*np.sin(2.*np.pi*ti/Tp)*np.cos((np.pi*(r-0.6*L))/(0.6*L))

  ice_h_soln = Hs+P

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
  file.climate_name = 'bueler_isothermal_d'
  file.climate_param__var = [H0, L, Cp, Tp, q0, A]
  file.ice_bc_name__nesw = 'no_ice,no_ice,no_flux,no_flux'
  file.ice_soln_name = 'bueler_isothermal_d'
  file.ice_soln_param__var = [H0, L, Cp, Tp]
  file.write_ice_h = 1
  file.write_ice_h_soln = 1 
  file.write_ice_q_surf = 1
  file.variables['x'][:] = xy
  file.variables['y'][:] = xy
  file.variables['topo'][:,:] = 0.
  file.variables['ice_h'][:,:] = ice_h_soln
  file.variables['ice_h_soln'][:,:] = ice_h_soln
  
  # write data specific to each flow method
  if flow == 'hindmarsh2_explicit':
    file.ice_name = 'hindmarsh2_explicit'
    file.ice_param__var = [A]
    file.write_ice_uv_defm = 1

  elif flow == 'hindmarsh2_sliding1_explicit':
    file.ice_name = 'hindmarsh2_sliding1_explicit'
    file.ice_param__var = []
    file.variables['ice_a_defm'][:,:] = A
    file.variables['ice_a_slid'][:,:] = 0.

  # finalize
  file.close()


if __name__ == '__main__':

  (n, flow, name) = ict.test_read_args()
  create(n, flow, name)
