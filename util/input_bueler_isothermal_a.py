#!/usr/bin/env python
#
# Generate ICE-CASCADE input file for Test A in (1). Grid spacing is an
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
# Keith Ma, April 2015

import sys
import numpy as np
import netCDF4 as nc
import ice_cascade_tools as ict

# parameters for Bueler test A 
M0 = 0.3 # surface ice flux, [m_ice/a] 
L = 7.5e5 # ice cap radius, [m]
g = 9.81 # acceleration of gravity, [m/s2]
rhoi = 910. # ice density, [kg/m3]
A = 1.0e-16 # ice deformation coeff, [Pa-3 a-1]

# general parameters
lxy = L # domain dimensions
ti = 0. # model start time
tf = 25000. # model end time
dt = 100. # model time step, should be irrelevant
tw = 25000. # output steps
descr = ('Benchmark case with exact solution (Bueler et al 2005, test A).'
  'Isothermal, non-sliding, steady state with fixed margin position and'
  'constant, positive surface ice flux.')

# main function
def main(filename, nxy):
  
  # coordinate grid
  (xy, dxy) = np.linspace(0.0, lxy, num = nxy, retstep = True, dtype = np.float64)
  
  # exact solution
  (xx, yy) = np.meshgrid(xy, xy)
  rr = np.sqrt(xx**2+yy**2)
  gamma = 2.0/5.0*A*(rhoi*g)**3.0
  mask = np.where(rr <= L)
  ice_h_soln = np.zeros((nxy,nxy), dtype = np.float64)
  ice_h_soln[mask] = (4.0*M0/gamma)**(1.0/8.0)*(L**(4.0/3.0)-rr[mask]**(4.0/3.0))**(3.0/8.0)
  
  # create and open new input file
  file = ict.new_input(filename, nxy, nxy)
  
  # define parameters and variables 
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
  file.ice_name = 'hindmarsh2_sliding_explicit'
  file.ice_param__var = []
  #file.ice_name = 'hindmarsh2_explicit'
  #file.ice_param__var = [A]
  file.ice_bc_name__nesw = 'no_ice,no_ice,no_flux,no_flux'
  file.ice_soln_name = 'bueler_isothermal_a'
  file.ice_soln_param__var = [M0, L, A]
  file.write_ice_q_surf = 1
  file.write_ice_h = 1
  file.write_ice_h_soln = 1 
  file.write_ice_h_dot = 1
  file.variables['x'][:] = xy
  file.variables['y'][:] = xy
  file.variables['topo'][:,:] = 0.
  file.variables['ice_h'][:,:] = ice_h_soln
  file.variables['ice_h_soln'][:,:] = ice_h_soln
  file.variables['ice_a_defm'][:,:] = A
  file.variables['ice_a_slid'][:,:] = 0.
  
  # finalize
  file.close()

if __name__ == '__main__':

  # defaults
  nxy = 50 
  filename = 'bueler_isothermal_a_in_'+str(nxy)+'.nc' 
  
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
