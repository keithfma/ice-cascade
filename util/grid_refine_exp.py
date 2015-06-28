#!/usr/bin/env python
#
# Grid refinement experiment comparing the ice model numerical results against
# known exact solutions. The program provides utilities to setup, run, and
# analyze one of the included test cases. 
#
# Usage: ./grid_refine_exp.py test_name flow_name out_dir nxy
#
# Command line arguments:
#
#   test_name = String. The name of the test case. Must be one of the following
#     options:
#
#       bueler_isothermal_a
#       bueler_isothermal_a_full
#       bueler_isothermal_b
#       bueler_isothermal_b_full
#       bueler_isothermal_c
#       bueler_isothermal_c_full
#       bueler_isothermal_d
#       bueler_isothermal_d_full
#       bueler_isothermal_e
#
#   flow_name = String. The name of the ice flow method to be used for
#     the test. Note that there is no gaurentee that the test_*.create function
#     has been written to properly select the correct method. Must be one of the
#     following methods:
#     
#       hindmarsh2_explicit
#       hindmarsh2_sliding_explicit
#
#   out_dir = String. Path where the experiment output should be saved.
#
#   nxy = String. A list of the grid sizes to use, as comma separated values in
#     single quotes, e.g. '16,32,64'
#
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

import test_bueler_isothermal_a 
import test_bueler_isothermal_a_full 
#import input_bueler_isothermal_b 
#import input_bueler_isothermal_b_full 
#import input_bueler_isothermal_c 
#import input_bueler_isothermal_c_full 
#import input_bueler_isothermal_d 
#import input_bueler_isothermal_d_full 
#import input_bueler_isothermal_e 

def fit_power_law(x, y):
  '''Find the best-fit power law function of the form y = c*x**p for the data
  points in 'x' and 'y'. Return the best-fit parameters 'c' and 'p'.'''
  
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


def plot_data(x, y):
  '''Plot the data in 'x' and 'y' with a best-fit power law function of the form
  y = c*x**p.'''
  
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


def plot_map(z, d):
  '''Plot the array 'z' as an image. Coordinates are assumed to have thier
  origin in the lower left corner, and are set using the grid spacing 'd'. All
  units are assumed to be meters.'''

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

def read_args():
  '''Reads input parameters for the grid refinement experimant. Expected input
  arguments are:

    test_name = String. The name of the test case.
    flow_name = String. The name of the ice flow method to use. 
    nxy = String. A comma-separated list of grid dim in pts (e.g. '32,64').
    outdir = String, optional, default is 'out'. Path to save output.
  
  Returns the tuple (test_name, flow_name, nxy, outdir)''' 

  show_help = False
  help = ('''
  Usage: ./grid_refine_exp.py test_name flow_name nxy outdir

    test_name = String. The name of the test case.
    flow_name = String. The name of the ice flow method to use. 
    nxy = String. A comma-separated list of grid dim in pts (e.g. '32,64').
    outdir = String. Path to save outputs.
  ''')

  # check if user wants help
  if len(sys.argv) == 2 and sys.argv[1] == '--help':
    show_help = True

  # check if user needs help
  elif len(sys.argv) != 5:
    print('\nERROR: Incorrect number of input arguments.')
    show_help = True

  # parse input arguments
  else:
    try:
      test_name = sys.argv[1]
      flow_name = sys.argv[2]
      outdir = sys.argv[3]
      nxy = map(int, sys.argv[4].split(','))
    except:
      print('\nERROR: Failed to read input arguments.')
      show_help = True

  # show help and exit, if needed   
  if show_help:
    print(help)
    sys.exit()

  return (test_name, flow_name, outdir, nxy)



# setup and run experiment
if __name__ == '__main__':

  (test_name, flow_name, out_dir, nxy) = read_args()

  # select test
  if test_name == 'bueler_isothermal_a':
    create_input_file = test_bueler_isothermal_a.create
    title_str = 'Bueler et al 2005, Test A'

#  elif name == 'bueler_isothermal_a_full':
#    make_input = input_bueler_isothermal_a.main
#    title = 'Bueler et al 2005, Test A, Full Ice Cap'
#
#  #elif name == 'bueler_isothermal_b':
#  #  make_input = input_bueler_isothermal_b.main
#  #  title = 'Bueler et al 2005, Test B'
# 
#  #elif name == 'bueler_isothermal_b_full':
#  #  make_input = input_bueler_isothermal_b_full.main
#  #  title = 'Bueler et al 2005, Test B, Full Ice Cap'
#
#  #elif name == 'bueler_isothermal_c':
#  #  make_input = input_bueler_isothermal_c.main
#  #  title = 'Bueler et al 2005, Test C'
# 
#  #elif name == 'bueler_isothermal_c_full':
#  #  make_input = input_bueler_isothermal_c_full.main
#  #  title = 'Bueler et al 2005, Test C, Full Ice Cap'
#
#  #elif name == 'bueler_isothermal_d':
#  #  make_input = input_bueler_isothermal_d.main
#  #  title = 'Bueler et al 2005, Test D'
# 
#  #elif name == 'bueler_isothermal_d_full':
#  #  make_input = input_bueler_isothermal_d_full.main
#  #  title = 'Bueler et al 2005, Test D, Full Ice Cap'
#
#  #elif name == 'bueler_isothermal_e':
#  #  make_input = input_bueler_isothermal_e.main
#  #  title = 'Bueler et al 2005, Test E'
#
  else:
    print('\nERROR: Invalid test name.')
    sys.exit()

  # create output folder, if needed
  if not os.path.exists(out_dir):
    os.makedirs(out_dir)

  # figure out max number of digits in nxy list
  digits = 1
  while (max(nxy) >= 10**digits):
    digits = digits+1

  # init empty lists for results
  err_max = [] # max of abs(error)
  err_mean = [] # mean of abs(error)
  err_dome = [] # error at ice cap center

  # create input files, run models, compute errors, plot maps, gather stats
  for n in nxy:

    # generate names
    input_file  = test_name+'_'+str(n).zfill(digits)+'_in.nc'
    output_file = test_name+'_'+str(n).zfill(digits)+'_out.nc'
    errmap_file = test_name+'_'+str(n).zfill(digits)+'_fig_errmap.pdf'
    
    # names -> paths
    input_path = os.path.join(out_dir, input_file)
    output_path = os.path.join(out_dir, output_file)
    errmap_path = os.path.join(out_dir, errmap_file)

    # run model
    create_input_file(n, flow_name, input_path)
    subprocess.call(['ice-cascade', input_path, output_path]) 
  
    # read output ice thickness at the final timestep
    file = nc.Dataset(output_path, mode = 'r')
    nt = file.variables['t'].size
    dx = file.attributes['dx__m']
    h = file.variables['ice_h'][nt-1,:,:]
    h_soln = file.variables['ice_h_soln'][nt-1,:,:]
    file.close()
  
    # compute error, including map and scalar statistics
    mask = np.logical_or(np.greater(h, 0.), np.greater(h_soln, 0.)) 
    err = h_soln-h
    abserr = abs(err)
    err_max.append(abserr.max())
    err_mean.append(np.mean(abserr[mask]))
    idome, jdome = np.unravel_index(h_soln.argmax(), h_soln.shape)
    err_dome.append(abserr[idome,jdome])

    # plot difference map
    plot_map(err, dx/1000.)
    plt.title('N = {0:.0f}, Delta = {1:.0f} m'.format(n, dx))
    plt.savefig(errmap_path)
    plt.close()

  # generate names
  errplt_mean_file = test_name+'_fig_errplt_mean.pdf'
  errplt_max_file  = test_name+'_fig_errplt_max.pdf'
  errplt_dome_file = test_name+'_fig_errplt_dome.pdf'
  report_file = test_name+'_report.tex'
  
  # names -> paths
  errplt_mean_path = os.path.join(out_dir, errplt_mean_file)
  errplt_max_path  = os.path.join(out_dir, errplt_max_file)
  errplt_dome_path = os.path.join(out_dir, errplt_dome_file)
  report_path = os.path.join(out_dir, report_file)

  # plot mean absolute error
  plot_data(nxy, err_mean)
  plt.title('Mean Absolute Error')
  plt.ylabel('Mean absolute error')
  plt.savefig(errplt_mean_path)
  plt.close()

  # plot max absolute error
  plot_data(nxy, err_max)
  plt.title('Max Absolute Error')
  plt.ylabel('max absolute error')
  plt.savefig(errplt_max_path)
  plt.close()

  # plot dome absolute error
  plot_data(nxy, err_dome)
  plt.title('Dome Absolute Error')
  plt.ylabel('dome absolute error')
  plt.savefig(errplt_dome_path)
  plt.close()
  
  # create latex report including figures and tabulared scalars
#  with open(report_name, 'w') as f:
#    
#    f.write('\\documentclass[11pt]{article}\n')
#    f.write('\\usepackage[margin=1.0in]{geometry}\n')
#    f.write('\\usepackage{graphicx}\n')
#    f.write('\\usepackage{placeins}\n')
#    f.write('\\title{Grid Refinement Experiment: '+title+'}\n')
#    
#    f.write('\\begin{document}\n')
#    f.write('\\maketitle\n')
#  
#    f.write('\\section{Error Statistics}\n')
#  
#    f.write('\\begin{table}[h]\n')
#    f.write('\\begin{tabular}{rrrrr}\n')
#    f.write('$N$ & $\Delta$ & max$|err|$ & mean$|err|$ & $|err|$ dome \\\\\n')
#    f.write('\\hline\n')
#    for i in range(len(nxy)):
#      f.write('{0:.0f} & {1:.0f} & {2:.0f} & {3:.0f} & {4:.0f} \\\\\n'.format(
#        nxy[i], dx[i], err_abs_max[i], err_abs_mean[i], err_abs_dome[i]))
#    f.write('\\hline\n')
#    f.write('\\end{tabular}\n')
#    f.write('\\end{table}\n')
#  
#    f.write('\\FloatBarrier\n')
#    f.write('\\section{Error Maps}\n')
#  
#    for n in nxy:
#      fig_err_map_name = os.path.join(dir, name+'_err_'+str(n).zfill(pad)+'.pdf')
#      f.write('\\begin{figure}[h]\n')
#      f.write('\\centering\n')
#      f.write('\\centerline{\includegraphics{'+fig_err_map_name+'}}\n')
#      f.write('\\end{figure}\n')
#  
#    f.write('\\FloatBarrier\n')
#    f.write('\\section{Convergence Rate Estimates}\n')
#   
#    f.write('\\begin{figure}[h]\n')
#    f.write('\\centering\n')
#    f.write('\\centerline{\includegraphics{'+fig_err_abs_max_name+'}}\n')
#    f.write('\\end{figure}\n')
#    
#    f.write('\\begin{figure}[h]\n')
#    f.write('\\centering\n')
#    f.write('\\centerline{\includegraphics{'+fig_err_abs_mean_name+'}}\n')
#    f.write('\\end{figure}\n')
#  
#    f.write('\\begin{figure}[h]\n')
#    f.write('\\centering\n')
#    f.write('\\centerline{\includegraphics{'+fig_err_abs_dome_name+'}}\n')
#    f.write('\\end{figure}\n')
#    
#    f.write('\end{document}\n')
#  
#  # compile report
#  subprocess.call(['pdflatex', report_name])
#  subprocess.call(['pdflatex', report_name])
#  
#  # clean up working files
#  os.rename(name+'_report.pdf', os.path.join(dir, name+'_report.pdf'))
#  os.remove(name+'_report.aux')
#  os.remove(name+'_report.log')
#
