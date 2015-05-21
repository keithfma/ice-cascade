#!/usr/bin/env python
#
# Analyze results from grid refinement experiment for Test A in [1]. Expects one
# command line argument, the directory containing the output files.
#
# Usage: grid_refine_analyze.py dir
#
# References:
#
#   (1) Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
#   L.  N. (2005). Exact solutions and verification of numerical models for
#   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
#   doi:10.3189/172756505781829449
#
# Keith Ma, May 2015

import glob
import sys
import os
import numpy as np
import netCDF4 as nc
import matplotlib.pyplot as plt

# fit power law
def fit_power_law(x, y):
  
  x = np.matrix(x, dtype = np.float64)
  x = np.reshape(x, (x.size, 1))
  x = np.log10(x)
  one = np.ones((x.size,1))
  y = np.matrix(y, dtype = np.float64)
  y = np.reshape(y, (y.size, 1))
  y = np.log10(y)
  A = np.hstack([one, x])
  b = y

  (param, residuals, rank, s) = np.linalg.lstsq(A,b)
  c = 10.**param.item(0)
  p = param.item(1)

  return (c, p)

# plot data with power law fit
def plot_data(x, y):
  
  (c, p) = fit_power_law(x, y)
  xf = np.linspace(min(x), max(x), 100)
  yf = c*xf**p

  plt.figure()
  plt.loglog(x, y, 'o')
  plt.loglog(xf, yf)
  plt.xlim(min(x), max(x))
  plt.text(0.5, 0.9, 'y = {0:.2f} x ** {1:.2f}'.format(c, p),
    size = 14, horizontalalignment='center', verticalalignment='center', 
    transform = plt.gca().transAxes)
  plt.xlabel('N grid points')
  plt.grid(True, which = 'both')

  return 

# plot error maps
def plot_map(z, d):

  (nx, ny) = z.shape
  extr = np.amax(abs(z))

  plt.figure()
  plt.imshow(z, cmap = 'bwr', interpolation = 'none', 
    extent = (0., d*(nx-1), 0., d*(ny-1)), origin = 'lower', 
    vmin = -extr, vmax = extr) 
  plt.colorbar(label = 'Error, m')
  plt.xlabel('X-position, km')
  plt.ylabel('Y-position, km')
  
  return

# parse input arguments
if len(sys.argv) != 2:
  print 'Usage: grid_refine_analyze.py dir'
  sys.exit()
else: 
  dir = sys.argv[1]

# init empty lists
nx = [] # num grid points 
dx = [] # grid spacing, m
h = [] # final numerical ice thickness
h_soln = [] # final exact ice thickness
h_err = [] # error (exact = numerical)
h_err_abs = [] # abs(error)
h_err_abs_max = [] # max of abs(error)
h_err_abs_mean = [] # mean of abs(error)
h_err_abs_std = [] # standard deviation of abs(error)
h_err_abs_dome = [] # error at ice cap center

# loop over output files
files = glob.glob(dir+'/bueler_isothermal_a_out_*.nc')
for i in range(len(files)):

  # read data
  file = nc.Dataset(files[i], mode = 'r')
  nx.append(file.nx__1)
  dx.append(file.dx__m)
  h.append(file.variables['ice_h'][0,:,:])
  h_soln.append(file.variables['ice_h_soln'][0,:,:])
  file.close()

  # compute error, map and scalar statistics
  mask = np.logical_or(np.greater(h[i], 0.), np.greater(h_soln[i], 0.)) 
  h_err.append(h_soln[i]-h[i])
  h_err_abs.append(abs(h_err[i]))
  h_err_abs_max.append(h_err_abs[i].max())
  h_err_abs_mean.append(np.mean(h_err_abs[i][mask]))
  h_err_abs_std.append(np.std(h_err_abs[i][mask]))
  h_err_abs_dome.append(h_err_abs[i][0,0])

# plot mean absolute error
plot_data(nx, h_err_abs_mean)
plt.title('Bueler et al 2005, Test A, Mean Absolute Error')
plt.ylabel('Mean absolute error')
plt.savefig(dir+'/bueler_isothermal_a_err_abs_mean.pdf')
plt.close()

# plot max absolute error
plot_data(nx, h_err_abs_max)
plt.title('Bueler et al 2005, Test A, Max Absolute Error')
plt.ylabel('Max absolute error')
plt.savefig(dir+'/bueler_isothermal_a_err_abs_max.pdf')
plt.close()

# plot dome absolute error
plot_data(nx, h_err_abs_dome)
plt.title('Bueler et al 2005, Test A, Dome Absolute Error')
plt.ylabel('Dome absolute error')
plt.savefig(dir+'/bueler_isothermal_a_err_abs_dome.pdf')
plt.close()

# figure out padding
d = 1
while (max(nx) > 10**d):
  d = d+1

# plot difference maps
for i in range(len(h_err)):
  plot_map(h_err[i], dx[i]/1000.)
  plt.title('''Bueler et al 2005, Test A
      N = {0:.0f}, Delta = {1:.0f} m'''.format(nx[i], dx[i]))
  plt.savefig(dir+'/bueler_isothermal_a_err_'+str(nx[i]).zfill(d)+'.pdf')
  plt.close()

# create latex report including figures and tabulared scalars
with open(dir+'/bueler_isothermal_a_report.tex', 'w') as f:
  
  f.write('\documentclass[11pt]{article}\n')
  f.write('\usepackage[margin=1.0in]{geometry}\n')
  f.write('\usepackage{graphicx}\n')
  f.write('\usepackage{placeins}\n')
  f.write('\\title{Bueler et al, 2005 Test A, Grid Refinement Experiment}\n')
  
  f.write('\\begin{document}\n')
  f.write('\maketitle\n')

  f.write('\section{Error Statistics}\n')

  f.write('\\begin{table}[h]\n')
  f.write('\\begin{tabular}{rrrrr}\n')
  f.write('$N$ & $\Delta$ & max$|err|$ & mean$|err|$ & $|err|$ dome \\\\\n')
  f.write('\hline\n')
  for i in range(len(nx)):
    f.write('{0:.0f} & {1:.0f} & {2:.0f} & {3:.0f} & {4:.0f} \\\\\n'.format(
      nx[i], dx[i], h_err_abs_max[i], h_err_abs_mean[i], h_err_abs_dome[i]))
  f.write('\hline\n')
  f.write('\end{tabular}\n')
  f.write('\end{table}\n')

  f.write('\FloatBarrier\n')
  f.write('\section{Error Maps}\n')

  for i in range(len(nx)):
    pdf = dir+'/bueler_isothermal_a_err_'+str(nx[i]).zfill(d)+'.pdf'
    f.write('\\begin{figure}[h]\n')
    f.write('\centering\n')
    f.write('\centerline{\includegraphics{'+pdf+'}}\n')
    f.write('\end{figure}\n')

  f.write('\FloatBarrier\n')
  f.write('\section{Convergence Rate Estimates}\n')
 
  f.write('\\begin{figure}[h]\n')
  f.write('\centering\n')
  f.write('\centerline{\includegraphics{'+dir+'/bueler_isothermal_a_err_abs_max.pdf}}\n')
  f.write('\end{figure}\n')
  
  f.write('\\begin{figure}[h]\n')
  f.write('\centering\n')
  f.write('\centerline{\includegraphics{'+dir+'/bueler_isothermal_a_err_abs_mean.pdf}}\n')
  f.write('\end{figure}\n')

  f.write('\\begin{figure}[h]\n')
  f.write('\centering\n')
  f.write('\centerline{\includegraphics{'+dir+'/bueler_isothermal_a_err_abs_dome.pdf}}\n')
  f.write('\end{figure}\n')
  
  f.write('\end{document}\n')

# compile report
os.system('pdflatex '+dir+'/bueler_isothermal_a_report.tex')
os.system('pdflatex '+dir+'/bueler_isothermal_a_report.tex')

# clean up working files
os.rename('bueler_isothermal_a_report.pdf',
  dir+'/bueler_isothermal_a_report.pdf')
for f in glob.glob('bueler_isothermal_a_report.*'):
  os.remove(f)
  
