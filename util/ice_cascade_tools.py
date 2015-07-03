#!/usr/bin/env python
#
# Module containing useful functions for creating ICE-CASCADE input files.
#
# Keith Ma, April 2015

import sys
import numpy as np
import netCDF4 as nc

def new_input(filename, nx, ny):
  '''Create a new ICE-CASCADE input file with null values for all required
  parameters (required) and state variables (required and optional), returns an
  open netCDF4 Dataset object associated with the new file.''' 

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
  file.time_write__a = 0. # new: expects a vector giving times explicitly
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
  file.write_ice_uv_defm = 0
  file.write_ice_uv_slid = 0
  file.write_ice_a_defm = 0
  file.write_ice_a_slid = 0

  # create dimensions
  file.createDimension('x', size = nx)
  file.createDimension('y', size = ny)

  # write initial state variables as variables
  var = file.createVariable('x', np.float64, dimensions = ('x'))
  var[:] = np.nan
  var.long_name = 'x_coordinate'
  var.units = 'm'
 
  var = file.createVariable('y', np.float64, dimensions = ('y'))
  var[:] = np.nan
  var.long_name = 'y_coordinate'
  var.units = 'm'

  var = file.createVariable('topo', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'topography'
  var.units = 'm'
  
  var = file.createVariable('topo_dot_ice', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'topography_rate_of_change_from_ice_erosion_and_deposition'
  var.units = 'm_a'

  var = file.createVariable('surf', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'surface_elevation_including_ice'
  var.units = 'm'

  var = file.createVariable('temp_surf', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'temperature_at_surface_of_ice_or_bedrock'
  var.units = 'C'

  var = file.createVariable('temp_ice', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'mean_ice_temperature'
  var.units = 'C'

  var = file.createVariable('temp_base', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'temperature_at_surface_of_bedrock'
  var.units = 'C'

  var = file.createVariable('precip', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'precipitation_rate'
  var.units = 'mwater_a'

  var = file.createVariable('runoff', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'water_runoff_rate'
  var.units = 'mwater_a'

  var = file.createVariable('ice_q_surf', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'surface_ice_flux'
  var.units = 'mice_a'

  var = file.createVariable('ice_h', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'ice_thickness'
  var.units = 'm'

  var = file.createVariable('ice_h_dot', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'ice_thickness_rate_of_change'
  var.units = 'm_a'
  
  var = file.createVariable('ice_h_soln', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'ice_thickness_exact_solution'
  var.units = 'm'

  var = file.createVariable('ice_a_defm', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'ice_deformation_coefficient'
  var.units = '1_Pa3_a'

  var = file.createVariable('ice_a_slid', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'ice_sliding_coefficient'
  var.units = 'm_Pa_a'
  
  var = file.createVariable('ice_u_defm', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'ice_deformation_velocity_x_dir'
  var.units = 'm_a'

  var = file.createVariable('ice_v_defm', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'ice_deformation_velocity_y_dir'
  var.units = 'm_a'
  
  var = file.createVariable('ice_u_slid', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'ice_sliding_velocity_x_dir'
  var.units = 'm_a'

  var = file.createVariable('ice_v_slid', np.float64, dimensions = ('y', 'x'))
  var[:,:] = np.nan
  var.long_name = 'ice_sliding_velocity_y_dir'
  var.units = 'm_a'

  # return open Dataset object
  return file


def test_read_args():
  '''read input parameters for the test_* utility scripts, which are used to
  create ICE-CASCADE input files for various test cases. All of these share the
  same input arguments and usage. The expected input arguments are:

  nxy = Integer, num grid points in x- and y-dir
  flow_method = String, name of ice flow method 
  filename = String, name of generated input file
  
  Returns the tuple (nxy, flow_method, filename)''' 

  show_help = False
  help = ('''
Usage: ./test_*.py nxy flow_method filename 

  nxy = num grid points in x- and y-dir
  flow_method = name of ice flow method 
  filename = name of generated input file
''')

  # check if user wants help
  if len(sys.argv) == 2 and sys.argv[1] == '--help':
    show_help = True

  # check if user needs help
  elif len(sys.argv) != 4:
    print('\nERROR: Incorrect number of input arguments.')
    show_help = True

  # parse input arguments
  else:
    try:
      nxy = int(sys.argv[1])
      flow_method = sys.argv[2]
      filename = sys.argv[3]
    except:
      print('\nERROR: Failed to read input arguments.')
      show_help = True

  # show help and exit, if needed   
  if show_help:
    print(help)
    sys.exit()

  return (nxy, flow_method, filename)
