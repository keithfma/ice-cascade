#!/usr/bin/env python
#
# Generate model domain for the Rhone valley glacier modeling project from raw
# SRTM input data using GMT5 command line tools
#
# References:
#
#   Snyder, J. P. (1987). Map Projections: A Working Manual. U.S. Geological
#   Survey Professional Paper 1395. doi:10.2307/1774978 
#
# Keith Ma, April 2015

import sys
import math
import subprocess
import os

# Definitions
raw_file = "srtm/srtm_38_03.tif" # String. Filename for input SRTM
lon0 = 5.8 # Real. Longitude of origin (SW-corner), [deg E] 
lat0 = 45.8 # Real. Latitude of origin (SW-corner), [deg N] 
L = 210.e3 # Real. Length of domain in the E-W (x) dir, [m] 
H = 110.e3 # Real. Length of domain in the N-S (y) dir, [m] 
dx = 2000. # Real. Grid spacing in the E-W (x) dir, [m]
dy = 2000. # Real. Grid spacing in the N-W (y) dir, [m]
out_file = 'rhone_topo_x{:.0f}m_y{:.0f}m.nc'.format(dx, dy) # String. Filename for netcdf output

# Constants
rearth = 6371.e3 # Real. Mean radius of earth, [m]
rad2deg = 180/math.pi

# Check for sane inputs
if (L % dx) !=  0.: 
  print 'Inconsistent grid definition: L must be divisible by dx'
  sys.exit(1)

if (H % dy) !=  0.: 
  print 'Inconsistent grid definition: H must be divisible by dy'
  sys.exit(1)


# Compute standard parallels for Albers Equal Area projection
#   choice of projection and std parallels follows recommendations in Snyder '87 
dlat = H/rearth*rad2deg
lat1 = lat0+(1./6.)*dlat
lat2 = lat0+(5./6.)*dlat

# Compute sigma/width for Gaussian anti-aliasing filter 
#   computations follow http://en.wikipedia.org/wiki/Gaussian_filter
fc = 1/max(dx,dy)
c = 2
sigmaf = fc/math.sqrt(2*math.log(2))
sigma = 1/(2*math.pi*sigmaf)

# GMT: convert from geotiff to GMT netcdf
subprocess.call(['grdreformat', raw_file+'=gd', '-Vl', 'step1.nc'])

# GMT: apply gaussian anti-aliasing filter
subprocess.call(['grdfilter', 'step1.nc', '-D4','-Fg'+format(6*sigma/1000, '.3f'),
                 '-Vl', '-Gstep2.nc'])  

# GMT: project to cartesian coords and clip extent
subprocess.call( ['grdproject', 'step2.nc', '-G'+out_file, '-Ae', '-Vl', 
                  '-Jb{:f}/{:f}/{:f}/{:f}/1:1'.format(lon0, lat0, lat1, lat2), 
                  '-D{:f}=/{:f}='.format(dx,dy), 
                  '-Re{:f}/{:f}/{:f}/{:f}'.format(0., L, 0., H)] )

# Delete temporary files
os.remove('step1.nc')
os.remove('step2.nc')
