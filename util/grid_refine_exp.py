#!/usr/bin/env python
#
# Grid refinement experiment comparing the ice model numerical results against
# known exact solutions. The program provides utilities to setup, run, and
# analyze one of the included test cases. 
#
# Usage: ./grid_refine_exp.py name nxy dir
#
# Command line arguments:
#
#   name = String, required. The name of the test case. Must be one of the
#     following options:
#
#       bueler_isothermal_a
#       bueler_isothermal_b
#       bueler_isothermal_b_full
#       bueler_isothermal_c
#       bueler_isothermal_c_full
#       bueler_isothermal_d
#       bueler_isothermal_d_full
#       bueler_isothermal_e
#
#   nxy = String, optional, default is '32,64'. A list of the grid sizes to
#     use, as comma separated values in single quotes, e.g. '16,32,64'
#
#   dir = String, optional, default is 'out'. Path where the experiment
#     output should be saved.
#
# References:
#   [1] Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
#   L.  N. (2005). Exact solutions and verification of numerical models for
#   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
#   doi:10.3189/172756505781829449
#
# Keith Ma, May 2015

import sys
import os
import subprocess
import numpy as np
import netCDF4 as nc
import matplotlib.pyplot as plt

import input_bueler_isothermal_a 
import input_bueler_isothermal_b 
import input_bueler_isothermal_b_full 
import input_bueler_isothermal_c 
import input_bueler_isothermal_c_full 
import input_bueler_isothermal_d 
import input_bueler_isothermal_d_full 
import input_bueler_isothermal_e 

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


# setup and run experiment
if __name__ == '__main__':

  # defaults
  name = ''
  nxy = [30, 60] 
  dir = 'out'
  
  # parse command line arguments
  narg = len(sys.argv)-1

  if narg < 1:
    print('Missing required input argument "name"')
    sys.exit(-1)
  name = sys.argv[1]
  
  if narg >= 2:
    nxy = [int(x) for x in sys.argv[2].split(',')]

  if narg >= 3:
    dir = sys.argv[3]

  if narg >= 4:
    print('Too many input arguments.')
    sys.exit(-1)

  # select test
  if name == 'bueler_isothermal_a':
    make_input = input_bueler_isothermal_a.main
    title = 'Bueler et al 2005, Test A'

  elif name == 'bueler_isothermal_b':
    make_input = input_bueler_isothermal_b.main
    title = 'Bueler et al 2005, Test B'
 
  elif name == 'bueler_isothermal_b_full':
    make_input = input_bueler_isothermal_b_full.main
    title = 'Bueler et al 2005, Test B, Full Ice Cap'

  elif name == 'bueler_isothermal_c':
    make_input = input_bueler_isothermal_c.main
    title = 'Bueler et al 2005, Test C'
 
  elif name == 'bueler_isothermal_c_full':
    make_input = input_bueler_isothermal_c_full.main
    title = 'Bueler et al 2005, Test C, Full Ice Cap'

  elif name == 'bueler_isothermal_d':
    make_input = input_bueler_isothermal_d.main
    title = 'Bueler et al 2005, Test D'
 
  elif name == 'bueler_isothermal_d_full':
    make_input = input_bueler_isothermal_d_full.main
    title = 'Bueler et al 2005, Test D, Full Ice Cap'

  elif name == 'bueler_isothermal_e':
    make_input = input_bueler_isothermal_e.main
    title = 'Bueler et al 2005, Test E'

  else:
    print('Invalid test name.')
    sys.exit(-1)

  # create output folder, if needed
  if not os.path.exists(dir):
    os.makedirs(dir)

  # figure out number of leading zeros
  pad = 1
  while (max(nxy) > 10**pad):
    pad = pad+1

  # init empty lists for results
  h = [] # final numerical ice thickness
  h_soln = [] # final exact ice thickness
  err = [] # error (exact = numerical)
  err_abs = [] # abs(error)
  err_abs_max = [] # max of abs(error)
  err_abs_mean = [] # mean of abs(error)
  err_abs_dome = [] # error at ice cap center
  dx = [] # grid spacing

  for n in nxy:

    # generate names
    input_name = os.path.join(dir, name+'_in_'+str(n).zfill(pad)+'.nc')
    output_name = os.path.join(dir, name+'_out_'+str(n).zfill(pad)+'.nc')
    fig_err_map_name = os.path.join(dir, name+'_err_'+str(n).zfill(pad)+'.pdf')

    # create input files using external routines
    make_input(input_name, n)

    # run model
    print(input_name)
    print(output_name)
    subprocess.call(['ice-cascade', input_name, output_name], 
                    stderr = sys.stdout.fileno() ) #subprocess.STDOUT)
  
    # read output ice thickness at the final timestep
    file = nc.Dataset(output_name, mode = 'r')
    nt = file.variables['t'].size
    dx.append(file.dx__m)
    h.append(file.variables['ice_h'][nt-1,:,:])
    h_soln.append(file.variables['ice_h_soln'][nt-1,:,:])
    file.close()
  
    # compute error, including map and scalar statistics
    mask = np.logical_or(np.greater(h[-1], 0.), np.greater(h_soln[-1], 0.)) 
    err.append(h_soln[-1]-h[-1])
    err_abs.append(abs(err[-1]))
    err_abs_max.append(err_abs[-1].max())
    err_abs_mean.append(np.mean(err_abs[-1][mask]))
    idome, jdome = np.unravel_index(h_soln[-1].argmax(), h_soln[-1].shape)
    err_abs_dome.append(err_abs[-1][idome,jdome])

    # plot difference map
    plot_map(err[-1], dx[-1]/1000.)
    plt.title('N = {0:.0f}, Delta = {1:.0f} m'.format(n, dx[-1]))
    plt.savefig(fig_err_map_name)
    plt.close()

  # plot mean absolute error
  fig_err_abs_mean_name = os.path.join(dir, name+'_err_abs_mean.pdf')
  plot_data(nxy, err_abs_mean)
  plt.title('Mean Absolute Error')
  plt.ylabel('Mean absolute error')
  plt.savefig(fig_err_abs_mean_name)
  plt.close()

  # plot max absolute error
  fig_err_abs_max_name = os.path.join(dir, name+'_err_abs_max.pdf')
  plot_data(nxy, err_abs_max)
  plt.title('Max Absolute Error')
  plt.ylabel('max absolute error')
  plt.savefig(fig_err_abs_max_name)
  plt.close()

  # plot dome absolute error
  fig_err_abs_dome_name = os.path.join(dir, name+'_err_abs_dome.pdf')
  plot_data(nxy, err_abs_dome)
  plt.title('Dome Absolute Error')
  plt.ylabel('dome absolute error')
  plt.savefig(fig_err_abs_dome_name)
  plt.close()
  
  # create latex report including figures and tabulared scalars
  report_name = os.path.join(dir, name+'_report.tex')
  with open(report_name, 'w') as f:
    
    f.write('\\documentclass[11pt]{article}\n')
    f.write('\\usepackage[margin=1.0in]{geometry}\n')
    f.write('\\usepackage{graphicx}\n')
    f.write('\\usepackage{placeins}\n')
    f.write('\\title{Grid Refinement Experiment: '+title+'}\n')
    
    f.write('\\begin{document}\n')
    f.write('\\maketitle\n')
  
    f.write('\\section{Error Statistics}\n')
  
    f.write('\\begin{table}[h]\n')
    f.write('\\begin{tabular}{rrrrr}\n')
    f.write('$N$ & $\Delta$ & max$|err|$ & mean$|err|$ & $|err|$ dome \\\\\n')
    f.write('\\hline\n')
    for i in range(len(nxy)):
      f.write('{0:.0f} & {1:.0f} & {2:.0f} & {3:.0f} & {4:.0f} \\\\\n'.format(
        nxy[i], dx[i], err_abs_max[i], err_abs_mean[i], err_abs_dome[i]))
    f.write('\\hline\n')
    f.write('\\end{tabular}\n')
    f.write('\\end{table}\n')
  
    f.write('\\FloatBarrier\n')
    f.write('\\section{Error Maps}\n')
  
    for n in nxy:
      fig_err_map_name = os.path.join(dir, name+'_err_'+str(n).zfill(pad)+'.pdf')
      f.write('\\begin{figure}[h]\n')
      f.write('\\centering\n')
      f.write('\\centerline{\includegraphics{'+fig_err_map_name+'}}\n')
      f.write('\\end{figure}\n')
  
    f.write('\\FloatBarrier\n')
    f.write('\\section{Convergence Rate Estimates}\n')
   
    f.write('\\begin{figure}[h]\n')
    f.write('\\centering\n')
    f.write('\\centerline{\includegraphics{'+fig_err_abs_max_name+'}}\n')
    f.write('\\end{figure}\n')
    
    f.write('\\begin{figure}[h]\n')
    f.write('\\centering\n')
    f.write('\\centerline{\includegraphics{'+fig_err_abs_mean_name+'}}\n')
    f.write('\\end{figure}\n')
  
    f.write('\\begin{figure}[h]\n')
    f.write('\\centering\n')
    f.write('\\centerline{\includegraphics{'+fig_err_abs_dome_name+'}}\n')
    f.write('\\end{figure}\n')
    
    f.write('\end{document}\n')
  
  # compile report
  subprocess.call(['pdflatex', report_name])
  subprocess.call(['pdflatex', report_name])
  
  # clean up working files
  os.rename(name+'_report.pdf', os.path.join(dir, name+'_report.pdf'))
  os.remove(name+'_report.aux')
  os.remove(name+'_report.log')

