#!/usr/bin/env python
#
# Generate ICE-CASCADE input file for test D in [1]. This 'full' variant
# computes the whole ice cap, as opposed to only the northeast quadrant as in
# [1]. Grid spacing is an (optional) command line argument to facilitate grid
# refinement experiments. To simplify calculation of 'dome' errors, the number
# of gridpoints must be odd so that the origin lies at a grid point.
#
# Usage:
#   ./make_input_bueler_isothermal_d filename nxy
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

# parameters for Bueler test C (see table 3, and equation 10)
H0 = 3600. # [m]
L = 750000. # [m]
Cp = 200. # [m]
Tp = 5000. # [a]
g = 9.81 # acceleration of gravity, [m/s2]
rhoi = 910. # ice density, [kg/m3]
A = 1.0e-16 # ice deformation coeff, [Pa-3 a-1]

# general parameters
ti = 0. # [a]
tf = ti+5.*Tp # [a]
dt = 1. # model time step
tw = np.linspace(ti, tf, 25) # output steps
lxy = 1.1*L # domain dimensions (final radius + 10%)
descr = ('Benchmark case with exact solution (Bueler et al 2005, test D).'
  'Isothermal, non-sliding, transient ice cap with oscillating surface ice '
  'flux')

# main function
def main(filename, nxy):

  # confirm that nxy is odd and greater than 1
  if (nxy%2 == 0) or (nxy < 3):
    print 'Invalid value for input parameter nxy, must be odd'
    sys.exit()
  
  # coordinate grid
  dxy = 2.*lxy/float(nxy-1)
  nhalf = (nxy-1)/2
  xy = dxy*np.arange(-nhalf, nhalf+1, 1, dtype = np.float64) 
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
  file.climate_name = 'bueler_isothermal_d'
  #file.climate_param__var = [0.]
  file.ice_name = 'hindmarsh2_explicit'
  file.ice_param__var = [A]
  file.ice_bc_name__nesw = 'no_ice,no_ice,no_ice,no_ice'
  file.ice_soln_name = 'bueler_isothermal_c'
  file.ice_soln_param__var = [H0, L, Cp, Tp]
  file.write_ice_h = 1
  file.write_ice_h_soln = 1 
  file.write_ice_q_surf = 1
  file.variables['x'][:] = xy
  file.variables['y'][:] = xy
  file.variables['ice_h'][:,:] = ice_h_soln
  file.variables['ice_h_soln'][:,:] = ice_h_soln
  
  # finalize
  file.close()

if __name__ == '__main__':

  # defaults
  nxy = 51 
  filename = 'bueler_isothermal_c_full_in_'+str(nxy)+'.nc' 
  
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
