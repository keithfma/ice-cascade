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

from matplotlib import pyplot as plt

# parameters for Bueler test E
M0 = 0.3 # [m/a]
L = 750000. # [m]
mumax = 2.5e-11*31556926. # [m/Pa/a]
r1 = 200000. # [m]
r2 = 700000. # [m]
th1 = 10.*np.pi/180. # [radians]
th2 = 40.*np.pi/180. # [radians]
g = 9.81 # acceleration of gravity, [m/s2]
rhoi = 910. # ice density, [kg/m3]
A = 1.0e-16 # ice deformation coeff, [Pa-3 a-1]

# general parameters
ti = 0. # [a]
tf = 25000. # [a]
dt = 100. # model time step
tw = tf # output steps
lxy = 1.1*L # domain dimensions (final radius + 10%)
descr = ('Benchmark case with exact solution (Bueler et al 2005, test E).'
  'Isothermal, steady-state ice cap with sliding in a ice-stream-like sector'
  'only.')

# main function
def main(filename, nxy):

  # coordinate grids
  (xy, dxy) = np.linspace(0.0, lxy, num = nxy, retstep = True, dtype = np.float64)
  (xx, yy) = np.meshgrid(xy, xy)
  rr = np.sqrt(xx**2+yy**2)
  thth = np.arctan2(yy, xx)

  # sliding coefficient
  mask = rr > r1
  mask = np.logical_and(mask, rr < r2)
  mask = np.logical_and(mask, thth > th1)
  mask = np.logical_and(mask, thth < th2)
  mumu = np.zeros((nxy, nxy), dtype = np.float64)
  mumu[mask] = mumax*( (4.*(rr[mask]-r1)*(r2-rr[mask])/((r2-r1)**2)) * 
                     (4.*(thth[mask]-th1)*(th2-thth[mask])/((th2-th1)**2)) )

  # exact solution
  gamma = 2.0/5.0*A*(rhoi*g)**3.0
  mask = np.where(rr <= L)
  ice_h_soln = np.zeros((nxy, nxy), dtype = np.float64)
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
  file.climate_name = 'bueler_isothermal_e'
  file.climate_param__var = [M0, L, A, mumax, r1, r2, th1, th2]
  file.ice_name = 'none'
  file.ice_param__var = []
  file.ice_bc_name__nesw = 'no_ice,no_ice,no_flux,no_flux'
  file.ice_soln_name = 'bueler_isothermal_e'
  file.ice_soln_param__var = [M0, L, A]
  file.write_ice_q_surf = 1
  file.write_ice_h = 1
  file.write_ice_h_soln = 1 
  file.write_ice_a_slid = 1
  file.variables['x'][:] = xy
  file.variables['y'][:] = xy
  file.variables['topo'][:,:] = 0.
  file.variables['ice_h'][:,:] = ice_h_soln
  file.variables['ice_h_soln'][:,:] = ice_h_soln
  file.variables['ice_a_slid'][:,:] = mumu

  # # DEBUG
  # print mumu
  # plt.imshow(mumu, 
  #     origin = 'lower', 
  #     extent = (xy[0], xy[-1], xy[0], xy[-1]))
  # plt.colorbar()
  # plt.show()
  
# run as script
if __name__ == '__main__':

  # defaults
  nxy = 51 
  filename = 'bueler_isothermal_e_in_'+str(nxy)+'.nc' 
  
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
