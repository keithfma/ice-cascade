#!/usr/bin/env python
#
# Module containing useful functions for creating ICE-CASCADE input files.
#
# Keith Ma, April 2015

import numpy as np
import netCDF4 as nc

def new_input(filename, nx, ny):
  '''Create a new ICE-CASCADE input file with null parameters and state
  variables, returns an open netCDF4 Dataset object associated with the new
  file.''' 

  # open file
  file = nc.Dataset(filename, mode = 'w', clobber = True, format = 'NETCDF4')
  
  # write model parameters as global attributes
  file.descr = 'null' 
  file.nx__1 = 0  
  file.ny__1 = 0 
  file.lx__m = 0. 
  file.ly__m = 0. 
  file.dx__m = 0. 
  file.dy__m = 0. 
  file.rhoi__kg_m3 = 0. 
  file.grav__m_s2 = 0. 
  file.time_start__a = 0. 
  file.time_finish__a = 0. 
  file.time_step__a = 0. 
  file.time_step_write__a = 0. 
  file.climate_name = 'none' 
  file.climate_param__var = [] 
  file.ice_name = 'none' 
  file.ice_param__var = [] 
  file.ice_bc_name__nesw = 'none,none,none,none' 
  file.ice_soln_name = 'none' 
  file.ice_soln_param__var = [] 
  file.write_topo = 0 
  file.write_topo_dot_ice = 0  
  file.write_surf = 0
  file.write_temp_surf = 0
  file.write_temp_ice = 0
  file.write_temp_base = 0
  file.write_precip = 0
  file.write_runoff = 0
  file.write_ice_q_surf = 0
  file.write_ice_h = 0
  file.write_ice_h_dot = 0
  file.write_ice_h_soln = 0
  file.write_ice_uvd = 0
  file.write_ice_uvs = 0

  # create null arrays
  zero_x = np.zeros(nx, dtype = np.float64) 
  zero_y = np.zeros(ny, dtype = np.float64) 
  zero_xy = np.zeros((nx, ny), dtype = np.float64) 

  # create dimensions
  file.createDimension('x', size = nx)
  file.createDimension('y', size = ny)

  # write initial state variables as variables
  var = file.createVariable('x', np.float64, dimensions = ('x'))
  var[:] = zero_x
  var.long_name = 'x_coordinate'
  var.units = 'm'
 
  var = file.createVariable('y', np.float64, dimensions = ('y'))
  var[:] = zero_y
  var.long_name = 'y_coordinate'
  var.units = 'm'

  var = file.createVariable('topo', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'topography'
  var.units = 'm'
  
  var = file.createVariable('topo_dot_ice', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'topography_rate_of_change_from_ice_erosion_and_deposition'
  var.units = 'm_a'

  var = file.createVariable('surf', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'surface_elevation_including_ice'
  var.units = 'm'

  var = file.createVariable('temp_surf', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'temperature_at_surface_of_ice_or_bedrock'
  var.units = 'C'

  var = file.createVariable('temp_ice', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'mean_ice_temperature'
  var.units = 'C'

  var = file.createVariable('temp_base', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'temperature_at_surface_of_bedrock'
  var.units = 'C'

  var = file.createVariable('precip', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'precipitation_rate'
  var.units = 'mwater_a'

  var = file.createVariable('runoff', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'water_runoff_rate'
  var.units = 'mwater_a'

  var = file.createVariable('ice_q_surf', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'surface_ice_flux'
  var.units = 'mice_a'

  var = file.createVariable('ice_h', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'ice_thickness'
  var.units = 'm'

  var = file.createVariable('ice_h_dot', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'ice_thickness_rate_of_change'
  var.units = 'm_a'
  
  var = file.createVariable('ice_h_soln', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'ice_thickness_exact_solution'
  var.units = 'm'
  
  var = file.createVariable('ice_ud', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'ice_deformation_velocity_x_dir'
  var.units = 'm_a'

  var = file.createVariable('ice_vd', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'ice_deformation_velocity_y_dir'
  var.units = 'm_a'
  
  var = file.createVariable('ice_us', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'ice_sliding_velocity_x_dir'
  var.units = 'm_a'

  var = file.createVariable('ice_vs', np.float64, dimensions = ('x', 'y'))
  var[:,:] = zero_xy
  var.long_name = 'ice_sliding_velocity_y_dir'
  var.units = 'm_a'

  # return open Dataset object
  return file
