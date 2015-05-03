#!/usr/bin/env python
#
# Module containing useful functions for creating ICE-CASCADE input files.
#
# Keith Ma, April 2015

import numpy as np
import netCDF4 as nc

def null_param():
  '''Return a dictionary of null ICE-CASCADE parameters. 

  This is a convenience function to generate a data structure with all the
  required entries for the 'create' function.  The normal usage would be to call
  'null_param' and 'null_var', populate with the desired values, then call
  'create'.'''

  p = dict()
  p['descr'] = 'null' # model description
  p['nx'] = 0  # num grid points in x-dir, [1]
  p['ny'] = 0 # num grid points in y-dir, [1]
  p['lx'] = 0. # grid dim in x-dir, [m]
  p['ly'] = 0. # grid dim in y-dir, [m]
  p['dx'] = 0. # grid spacing in x-dir, [m]
  p['dy'] = 0. # grid spacing in y-dir, [m]
  p['rhoi'] = 0. # density of glacial ice, [kg/m3]
  p['grav'] = 0. # acceleration of gravity, [m/s2]
  p['time_start'] = 0. # start time [a]
  p['time_finish'] = 0. # finish time [a]
  p['time_step'] = 0. # time step [a]
  p['time_step_write'] = 0. # time step for output [a]
  p['climate_name'] = 'none' # climate method name
  p['climate_param'] = [] # climate model parameters, [various]
  p['ice_name'] = 'none' # ice method name
  p['ice_param'] = [] # ice model parameters, [various]

  return p


def null_var(nx, ny):
  '''Return a dictionary of null ICE-CASCADE state variables.

  This is a convenience funciton to generate a data structure with all of the
  required entries for the 'create' function. The normal usage would be to call
  'null_param' and 'null_var', populate with the desired values, then call
  'create'.'''

  zero2 = np.zeros((nx, ny), dtype = np.float64) 
  zero1x = np.zeros(nx, dtype = np.float64) 
  zero1y = np.zeros(ny, dtype = np.float64) 

  v = dict()

  # variables
  v['x'] = zero1x # x coordinate vector, [m]
  v['y'] = zero1y # y coordinate vector, [m]
  v['topo'] = zero2 # bedrock elevation, [m above sea level]
  v['topo_dot_ice'] = zero2 # topography rate-of-change due to glaciers, [m/a]
  v['temp_surf'] = zero2# temp at ice/bedrock surface, [C]
  v['temp_ice'] = zero2 # mean ice temperature, [C]
  v['temp_base'] = zero2 # temp at bedrock surface, [C]
  v['precip'] = zero2 # precipitation rate, [m_water/a]
  v['runoff'] = zero2 # runoff rate, [m_water/a]
  v['ice_q_surf'] = zero2 # surface ice flux, [m_ice/a]
  v['ice_h'] = zero2 # ice thickness, [m]
  v['ice_h_dot'] = zero2 # ice thickness rate-of-change, [m/a]
  v['ice_h_soln'] = zero2 # exact solution for ice thickness, [m]
  v['ice_ud'] = zero2 # ice deformation velocity, x-dir, [m/a]
  v['ice_vd'] = zero2 # ice deformation velocity, y-dir, [m/a]
  v['ice_us'] = zero2 # ice sliding velocity, x-dir, [m/a]
  v['ice_vs'] = zero2 # ice sliding velocity, y-dir, [m/a]

  # corresponding write flags
  v['write_topo'] = 0
  v['write_topo_dot_ice'] = 0
  v['write_temp_surf'] = 0
  v['write_temp_ice'] = 0
  v['write_temp_base'] = 0
  v['write_precip'] = 0
  v['write_runoff'] = 0
  v['write_ice_q_surf'] = 0
  v['write_ice_h'] = 0
  v['write_ice_h_dot'] = 0
  v['write_ice_h_soln'] = 0
  v['write_ice_ud'] = 0
  v['write_ice_vd'] = 0
  v['write_ice_us'] = 0
  v['write_ice_vs'] = 0

  return v


def create(filename, p, v):
  '''Create an input file for use with the ICE-CASCADE model.

  Input arguments are dictionaries containing all necessary model parameters (p)
  and initial state variables (v). The expected contents are the same as the
  dictionaries produced ny the 'null_var' and 'null_param' functions, and
  presumably modified to make a viable model configuration. The generated input
  is a netcdf file named 'filename' readable by ICE-CASCADE IO subroutines.'''

  # create file
  file = nc.Dataset(filename, mode = 'w', clobber = True)

  # write parameters as attributes
  file.nx__1 = p['nx']
  file.ny__1 = p['ny']
  file.lx__m = p['lx']
  file.ly__m = p['ly'] 
  file.dx__m = p['dx']
  file.dy__m = p['dy']
  file.rhoi__kg_m3 = p['rhoi']
  file.grav__m_s2 = p['grav']
  file.time_start__a = p['time_start']
  file.time_finish__a = p['time_finish']
  file.time_step__a = p['time_step']
  file.time_step_write__a = p['time_step_write']
  file.climate_name = p['climate_name']
  file.climate_param__var = p['climate_param']
  file.ice_name = p['ice_name']
  file.ice_param__var = p['ice_param']
  file.write_topo = v['write_topo']
  file.write_topo_dot_ice = v['write_topo_dot_ice']
  file.write_temp_surf = v['write_temp_surf']
  file.write_temp_ice = v['write_temp_ice']
  file.write_temp_base = v['write_temp_base']
  file.write_precip = v['write_precip']
  file.write_runoff = v['write_runoff']
  file.write_ice_q_surf = v['write_ice_q_surf']
  file.write_ice_h = v['write_ice_h']
  file.write_ice_h_dot = v['write_ice_h_dot']
  file.write_ice_h_soln = v['write_ice_h_soln']
  file.write_ice_ud = v['write_ice_ud']
  file.write_ice_vd = v['write_ice_vd'] 
  file.write_ice_us = v['write_ice_us']
  file.write_ice_vs = v['write_ice_vs'] 

  # create dimensions
  dim_x = file.createDimension('x', size = p['nx'])
  dim_y = file.createDimension('y', size = p['ny'])

  # write data
  var = file.createVariable('x', v['x'].dtype.str, dimensions = ('x'))
  var[:] = v['x']
  var.long_name = 'x_coordinate'
  var.units = 'm'
 
  var = file.createVariable('y', v['y'].dtype.str, dimensions = ('y'))
  var[:] = v['y']
  var.long_name = 'y_coordinate'
  var.units = 'm'

  var = file.createVariable('topo', v['topo'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['topo']
  var.long_name = 'topography'
  var.units = 'm'
  
  var = file.createVariable('topo_dot_ice', v['topo_dot_ice'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['topo_dot_ice']
  var.long_name = 'topography_rate_of_change_from_ice_erosion_and_deposition'
  var.units = 'm_a'

  var = file.createVariable('temp_surf', v['temp_surf'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['temp_surf']
  var.long_name = 'temperature_at_surface_of_ice_or_bedrock'
  var.units = 'C'

  var = file.createVariable('temp_ice', v['temp_ice'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['temp_ice']
  var.long_name = 'mean_ice_temperature'
  var.units = 'C'

  var = file.createVariable('temp_base', v['temp_base'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['temp_base']
  var.long_name = 'temperature_at_surface_of_bedrock'
  var.units = 'C'

  var = file.createVariable('precip', v['precip'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['precip']
  var.long_name = 'precipitation_rate'
  var.units = 'mwater_a'

  var = file.createVariable('runoff', v['runoff'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['runoff']
  var.long_name = 'water_runoff_rate'
  var.units = 'mwater_a'

  var = file.createVariable('ice_q_surf', v['ice_q_surf'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['ice_q_surf']
  var.long_name = 'surface_ice_flux'
  var.units = 'mice_a'

  var = file.createVariable('ice_h', v['ice_h'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['ice_h']
  var.long_name = 'ice_thickness'
  var.units = 'm'

  var = file.createVariable('ice_h_dot', v['ice_h_dot'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['ice_h_dot']
  var.long_name = 'ice_thickness_rate_of_change'
  var.units = 'm_a'

  var = file.createVariable('ice_h_soln', v['ice_h_soln'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['ice_h_soln']
  var.long_name = 'ice_thickness_exact_solution'
  var.units = 'm'

  var = file.createVariable('ice_ud', v['ice_ud'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['ice_ud']
  var.long_name = 'ice_deformation_velocity_x_dir'
  var.units = 'm_a'

  var = file.createVariable('ice_vd', v['ice_vd'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['ice_vd']
  var.long_name = 'ice_deformation_velocity_y_dir'
  var.units = 'm_a'

  var = file.createVariable('ice_us', v['ice_us'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['ice_us']
  var.long_name = 'ice_sliding_velocity_x_dir'
  var.units = 'm_a'

  var = file.createVariable('ice_vs', v['ice_vs'].dtype.str, dimensions = ('x', 'y'))
  var[:,:] = v['ice_vs']
  var.long_name = 'ice_sliding_velocity_y_dir'
  var.units = 'm_a'

  ## definition mode -> data mode

  ## populate variables

  # finalize file
  file.close()

  return
