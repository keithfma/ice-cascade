#!/usr/bin/env python
#
# Grid refinement experiment comparing the ice model numerical results against
# known exact solutions. Sets up, runs, and analyzes one of the included test
# cases. 
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
# Keith Ma, June 2015

import sys
import os
import subprocess
import numpy as np
import netCDF4 as nc
import matplotlib.pyplot as plt

import test_bueler_isothermal_a 
import test_bueler_isothermal_a_full 
import test_bueler_isothermal_b 
import test_bueler_isothermal_b_full 
import test_bueler_isothermal_c 
import test_bueler_isothermal_c_full 
import test_bueler_isothermal_d 
import test_bueler_isothermal_d_full 
import test_bueler_isothermal_e 

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
  '''Plot the array of error magnitude 'z' in meters as an image. Coordinates
  are assumed to have thier origin in the lower left corner, and are set using
  the grid spacing 'd' in km. All units are assumed to be meters.'''

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
  Usage: ./grid_refine_exp.py test_name flow_name out_dir nxy

    test_name = String. The name of the test case.
    flow_name = String. The name of the ice flow method to use. 
    out_dir = String. Path to save outputs.
    nxy = String. A comma-separated list of grid dim in pts (e.g. '32,64').
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
  num_runs = len(nxy)

  # select test
  if test_name == 'bueler_isothermal_a':
    create_input_file = test_bueler_isothermal_a.create
    title_str = 'Bueler et al 2005, Test A'

  elif test_name == 'bueler_isothermal_a_full':
    create_input_file = test_bueler_isothermal_a_full.create
    title_str = 'Bueler et al 2005, Test A, Full Ice Cap'

  elif test_name == 'bueler_isothermal_b':
    create_input_file = test_bueler_isothermal_b.create
    title_str = 'Bueler et al 2005, Test B'
 
  elif test_name == 'bueler_isothermal_b_full':
    create_input_file = test_bueler_isothermal_b_full.create
    title_str = 'Bueler et al 2005, Test B, Full Ice Cap'

  elif test_name == 'bueler_isothermal_c':
    create_input_file = test_bueler_isothermal_c.create
    title_str = 'Bueler et al 2005, Test C'
 
  elif test_name == 'bueler_isothermal_c_full':
    create_input_file = test_bueler_isothermal_c_full.create
    title_str = 'Bueler et al 2005, Test C, Full Ice Cap'

  elif test_name == 'bueler_isothermal_d':
    create_input_file = test_bueler_isothermal_d.create
    title_str = 'Bueler et al 2005, Test D'
 
  elif test_name == 'bueler_isothermal_d_full':
    create_input_file = test_bueler_isothermal_d_full.create
    title_str = 'Bueler et al 2005, Test D, Full Ice Cap'

  elif test_name == 'bueler_isothermal_e':
    create_input_file = test_bueler_isothermal_e.create
    title_str = 'Bueler et al 2005, Test E'

  else:
    print('\nERROR: Invalid test name.')
    sys.exit()

  # declare empty lists to be appended
  input_paths = []
  output_paths = []
  errmap_paths = [] 
  err_max = []
  err_mean = []
  err_dome = []
  dx = []

  # generate file names
  digits = 1
  while (max(nxy) >= 10**digits):
    digits = digits+1

  for i in range(len(nxy)):
    test_flow_n = test_name+'_'+flow_name+'_n'+str(nxy[i]).zfill(digits)
    input_paths.append( os.path.join(out_dir, test_flow_n+'_in.nc'))
    output_paths.append(os.path.join(out_dir, test_flow_n+'_out.nc'))
    errmap_paths.append(os.path.join(out_dir, test_flow_n+'_fig_errmap.pdf'))

  test_flow = test_name+'_'+flow_name
  errplt_mean_path = os.path.join(out_dir, test_flow+'_fig_errplt_mean.pdf')
  errplt_max_path  = os.path.join(out_dir, test_flow+'_fig_errplt_max.pdf')
  errplt_dome_path = os.path.join(out_dir, test_flow+'_fig_errplt_dome.pdf')
  report_path      = os.path.join(out_dir, test_flow+'_report.tex')

  # create output folder, if needed
  if not os.path.exists(out_dir):
    os.makedirs(out_dir)

  # loop over grid resolutions
  for i in range(len(nxy)):

    # run model
    create_input_file(nxy[i], flow_name, input_paths[i])
    subprocess.call(['ice-cascade', input_paths[i], output_paths[i]]) 
  
    # read output ice thickness at the final timestep
    file = nc.Dataset(output_paths[i], mode = 'r')
    nt = file.variables['t'].size
    dx.append(file.dx__m)
    h = file.variables['ice_h'][nt-1,:,:]
    h_soln = file.variables['ice_h_soln'][nt-1,:,:]
    file.close()
  
    # compute error map and scalar statistics
    mask = np.logical_or(np.greater(h, 0.), np.greater(h_soln, 0.)) 
    err = h_soln-h
    abserr = abs(err)
    err_max.append(abserr.max())
    err_mean.append(np.mean(abserr[mask]))
    idome, jdome = np.unravel_index(h_soln.argmax(), h_soln.shape)
    err_dome.append(abserr[idome,jdome])

    # plot error map
    plot_map(err, dx[i]/1000.)
    plt.title('N = {0:.0f}, Delta = {1:.0f} m'.format(nxy[i], dx[i]))
    plt.savefig(errmap_paths[i])
    plt.close()

  # plot mean absolute error
  plot_data(nxy, err_mean)
  plt.title('Mean Absolute Error')
  plt.ylabel('Mean absolute error')
  plt.savefig(errplt_mean_path)
  plt.close()

  # plot max absolute error
  plot_data(nxy, err_max)
  plt.title('Max Absolute Error')
  plt.ylabel('Max Absolute Error')
  plt.savefig(errplt_max_path)
  plt.close()

  # plot dome absolute error
  plot_data(nxy, err_dome)
  plt.title('Dome Absolute Error')
  plt.ylabel('Dome Absolute Error')
  plt.savefig(errplt_dome_path)
  plt.close()
  
  # create latex report including figures and tabulared scalars
  with open(report_path, 'w') as f:
    
    f.write('\\documentclass[11pt]{article}\n')
    f.write('\\usepackage[margin=1.0in]{geometry}\n')
    f.write('\\usepackage{graphicx}\n')
    f.write('\\usepackage{placeins}\n')
    f.write('\\title{Grid Refinement Experiment: '+title_str+'}\n')
    
    f.write('\\begin{document}\n')
    f.write('\\maketitle\n')
  
    f.write('\\section{Error Statistics}\n')
  
    f.write('\\begin{table}[h]\n')
    f.write('\\begin{tabular}{rrrrr}\n')
    f.write('$N$ & $\Delta$ & max$|err|$ & mean$|err|$ & $|err|$ dome \\\\\n')
    f.write('\\hline\n')
    for i in range(len(nxy)):
      f.write('{0:.0f} & {1:.0f} & {2:.0f} & {3:.0f} & {4:.0f} \\\\\n'.format(
        nxy[i], dx[i], err_max[i], err_mean[i], err_dome[i]))
    f.write('\\hline\n')
    f.write('\\end{tabular}\n')
    f.write('\\end{table}\n')
  
    f.write('\\FloatBarrier\n')
    f.write('\\section{Error Maps}\n')
  
    for i in range(len(nxy)):
      f.write('\\begin{figure}[h]\n')
      f.write('\\centering\n')
      f.write('\\centerline{\includegraphics{'+errmap_paths[i]+'}}\n')
      f.write('\\end{figure}\n')
  
    f.write('\\FloatBarrier\n')
    f.write('\\section{Convergence Rate Estimates}\n')
   
    f.write('\\begin{figure}[h]\n')
    f.write('\\centering\n')
    f.write('\\centerline{\includegraphics{'+errplt_max_path+'}}\n')
    f.write('\\end{figure}\n')
    
    f.write('\\begin{figure}[h]\n')
    f.write('\\centering\n')
    f.write('\\centerline{\includegraphics{'+errplt_mean_path+'}}\n')
    f.write('\\end{figure}\n')
  
    f.write('\\begin{figure}[h]\n')
    f.write('\\centering\n')
    f.write('\\centerline{\includegraphics{'+errplt_dome_path+'}}\n')
    f.write('\\end{figure}\n')
    
    f.write('\end{document}\n')
  
  # compile report
  subprocess.call(['pdflatex', report_path])
  subprocess.call(['pdflatex', report_path])

  # cleanup after pdflatex
  report_stub = os.path.splitext(os.path.basename(report_path))[0]
  os.rename(os.path.join('.'    , report_stub+'.pdf'), 
            os.path.join(out_dir, report_stub+'.pdf'))
  os.remove(os.path.join('.', report_stub+'.aux'))
  os.remove(os.path.join('.', report_stub+'.log'))
