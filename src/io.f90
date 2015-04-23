! =============================================================================
! Input/output variables and procedures routines for ice-cascade.
!
! Contains:
!   io_type: Public, object for io vars and methods
!   
! ============================================================================

module io_mod

use kinds_mod, only: rp, rp_nc
use state_mod, only: state_type
use climate_mod, only: climate_type
use netcdf

implicit none
private
public :: io_type


  ! ---------------------------------------------------------------------------
  ! TYPE: input/output vars and procedures
  ! ---------------------------------------------------------------------------
  type io_type
    character(len=100) :: name_out ! output file name
    character(len=100) :: name_topo ! initial topography name
    character(len=100) :: name_ice_h ! initial topography name
    integer :: n_step ! counter for current output step
    real(rp) :: time_step ! interval btw outputs, [a]
    logical :: write_topo 
    logical :: write_temp_surf 
    logical :: write_precip 
    logical :: write_ice_q_surf 
    logical :: write_runoff 
  contains
    procedure, pass :: read_param ! read parameters from the input file
    procedure, pass :: read_initial_vals ! read initial values 
    procedure, pass :: create_output ! create output file
    procedure, nopass :: write_status ! print model status to screen
    procedure, pass :: write_output_step ! write step
  end type io_type


contains


  ! ---------------------------------------------------------------------------
  ! SUB: Read input parameters from file
  ! ---------------------------------------------------------------------------
  subroutine read_param(io, s, c)

    class(io_type), intent(out) :: io
    type(state_type), intent(out) :: s
    type(climate_type), intent(out) :: c

    character(len=100) :: infile, line
    integer :: msg

    ! Get input file name from command line
    select case (command_argument_count())
      case (1)
      	call get_command_argument(1,infile) 
      case default
      	print*,'ICE-CASCADE expects exactly 1 input argument'
      	stop -1
    end select
    
    ! Sanitize input file (drop comments and empty lines)
    open(54, file = trim(infile), status = 'old', iostat = msg)
    if (msg .ne. 0) then
    	print*, 'The input file ', trim(infile), ' does not exist, exiting.'
    	stop
    end if	
    open (55, status = 'scratch')
    do while (msg .ge. 0)
    	read (54, '(a)', iostat = msg) line
    	if ((line(1:1) .ne. '$') .and. (line(1:1) .ne. ' ')) write (55, '(a)') line		
    enddo 
    close (54)
    rewind (55)
    
    ! Read input parameters
    read(55, *) s%time_start
    read(55, *) s%time_finish
    read(55, *) s%time_step
    read(55, *) s%rhoi
    read(55, *) s%nx	
    read(55, *) s%ny
    read(55, *) s%dx
    read(55, *) s%dy	
    read(55, *) io%name_topo 
    read(55, *) io%name_ice_h
    read(55, *) c%on_temp_surf 
    read(55, *) c%name_temp_surf 
    read(55, *) c%param_temp_surf(:) 
    read(55, *) c%on_precip 
    read(55, *) c%name_precip
    read(55, *) c%param_precip(:) 
    read(55, *) c%on_ice_q_surf 
    read(55, *) c%name_ice_q_surf 
    read(55, *) c%param_ice_q_surf(:) 
    read(55, *) c%on_runoff 
    read(55, *) c%name_runoff 
    read(55, *) c%param_runoff(:) 
    read(55, *) io%name_out	
    read(55, *) io%time_step
    read(55, *) io%write_topo
    read(55, *) io%write_temp_surf
    read(55, *) io%write_precip
    read(55, *) io%write_ice_q_surf
    read(55, *) io%write_runoff

    ! Check for sane values
    ! positive grid dimensions 
    if ((s%nx .le. 0) .or. (s%ny .le. 0)) then
      print *, 'Invalid grid description: grid dimensions must be positive integers.'
      stop -1
    end if

    ! positive grid spacing
    if ((s%dx .le. 0.0_rp) .or. (s%dy .le. 0.0_rp)) then
      print *, 'Invalid grid description: grid spacings must be positive.'
      stop -1
    end if

    ! positive densities
    if (s%rhoi .le. 0.0_rp) then
      print *, 'Invalid physical constant: density must be positive.'
      stop -1
    end if

    ! non-zero timestep 
    if (s%time_step .eq. 0.0_rp) then
      print *, 'Invalid time parameters: time step cannot be zero.'
      stop -1
    end if

    ! forward model, start before finish
    if ((s%time_step .gt. 0.0_rp) .and. (s%time_start .ge. s%time_finish))  then
      print *, 'Invalid time parameters: start must be before finish for & 
               &forward models.'
      stop -1
    end if

    ! reverse model, finish before start
    if ((s%time_step .lt. 0.0_rp) .and. (s%time_start .le. s%time_finish)) then
      print *, 'Invalid time parameters: start must be after finish for &
               &reverse models.'
      stop -1
    end if

    ! output interval matches time step
    if (mod(io%time_step, s%time_step) .ne. 0.0_rp) then
      print *, 'Invalid time parameters: output interval must be a multiple &
               &of the time step.'
      stop -1
    end if


  end subroutine read_param


  ! ---------------------------------------------------------------------------
  ! SUB: read the initial values for state variables
  ! TO-DO: add initial ice thickness
  ! ---------------------------------------------------------------------------
  subroutine read_initial_vals(io, s)

    class(io_type), intent(in) :: io 
    type(state_type), intent(inout) :: s

    ! initial topography
    select case(io%name_topo)
    case('zero')
      s%topo = 0.0_rp
    case default
      call read_array_nc(io%name_topo, s%nx, s%ny, s%topo)
    end select

    ! initial ice thickness
    select case(io%name_ice_h)
    case('zero')
      s%ice_h = 0.0_rp
    case default
      call read_array_nc(io%name_ice_h, s%nx, s%ny, s%ice_h)
    end select

  end subroutine read_initial_vals


  ! --------------------------------------------------------------------------- 
  ! SUB: read 2D array from GMT grd format netcdf
  ! --------------------------------------------------------------------------- 
  subroutine read_array_nc(filename, nx, ny, array)
    
    character(len=*), intent(in) :: filename
    integer, intent(in) :: nx, ny
    real(rp), intent(out) :: array(:,:)

    integer :: ndim, nvar, n1, n2, ind, i, var_ndim
    integer :: msg, id_file

    ! open file
	  msg = nf90_open(trim(filename), nf90_nowrite, id_file)

    ! check dimensions
	  msg = nf90_inquire(id_file, ndim, nvar)	
	  if (ndim .ne. 2) then
		  print *,'Invalid input data: netcdf file should have only two dimensions'
      stop -1
    end if	
		msg = nf90_inquire_dimension(id_file, 1, len = n1)	
		if (n1 .ne. nx) then
      print *,'Invalid input data: data does not match the given dimensions (nx)'
      stop -1
    end if
		msg = nf90_inquire_dimension(id_file, 2, len = n2)	
		if (n2 .ne. ny) then
      print *,'Invalid input data: data does not match the given dimensions (ny)'
      stop -1
    end if
    
    ! identify array variable 
    ind = -1
    do i = 1, nvar
			msg = nf90_inquire_variable(id_file, i, ndims = var_ndim)
		  if ((var_ndim .eq. 2) .and. (ind .ne. -1)) then
				print*,'Invalid input data: netcdf file should only have one 2D variable'
        stop -1
      end if
      if (var_ndim .eq. 2) ind = i
  	end do	

    ! read array to interior points
	  msg = nf90_get_var(id_file, ind, array(2:nx+1, 2:ny+1))
    
    ! close file
    msg = nf90_close(id_file)

  end subroutine read_array_nc


  ! ---------------------------------------------------------------------------
  ! SUB: create output netcdf file
  ! ---------------------------------------------------------------------------
  subroutine create_output(io, s, c)

    class(io_type), intent(in) :: io
    type(state_type), intent(in) :: s
    type(climate_type), intent(in) :: c

    logical :: shuf
    integer :: fchunk(3), i, j, defLvl, msg, id_file, id_dim_x, id_dim_y, &
               id_dim_t, id_var

    ! define compression and chunking parameters 
    defLvl = 1 ! compression, 0 = none, 9 = max, best value is 1
    fchunk = [s%nx, s%ny, 1]
    shuf = .true.

    ! create new file
    msg = nf90_create(io%name_out, nf90_netcdf4, id_file)

    ! write parameters as global attributes
    msg = nf90_put_att(id_file, nf90_global, 'time_start_time__a', s%time_start)
    msg = nf90_put_att(id_file, nf90_global, 'time_finish_time__a', s%time_finish)
    msg = nf90_put_att(id_file, nf90_global, 'time_step__a', s%time_step)
    msg = nf90_put_att(id_file, nf90_global, 'time_step_output__a', io%time_step)
    msg = nf90_put_att(id_file, nf90_global, 'density_ice__kg1m-3', s%rhoi)
    msg = nf90_put_att(id_file, nf90_global, 'grid_nx__1', s%nx)
    msg = nf90_put_att(id_file, nf90_global, 'grid_ny__1', s%ny)
    msg = nf90_put_att(id_file, nf90_global, 'grid_dx__m', s%dx)
    msg = nf90_put_att(id_file, nf90_global, 'grid_dy__m', s%dy)
    msg = nf90_put_att(id_file, nf90_global, 'topo_initial__name', io%name_topo)
    msg = nf90_put_att(id_file, nf90_global, 'ice_h_initial__name', io%name_ice_h)
    msg = nf90_put_att(id_file, nf90_global, 'climate_temp_surf_on__tf', merge(1, 0, c%on_temp_surf))
    if (c%on_temp_surf) then
      msg = nf90_put_att(id_file, nf90_global, 'climate_temp_surf__name', c%name_temp_surf)
      msg = nf90_put_att(id_file, nf90_global, 'climate_temp_param__various', c%param_temp_surf)
    end if
    msg = nf90_put_att(id_file, nf90_global, 'climate_precip_on__tf', merge(1, 0, c%on_precip))
    if (c%on_precip) then
      msg = nf90_put_att(id_file, nf90_global, 'climate_precip__name', c%name_precip)
      msg = nf90_put_att(id_file, nf90_global, 'climate_precip_param__various', c%param_precip)
    end if
    msg = nf90_put_att(id_file, nf90_global, 'climate_ice_q_surf_on__tf', merge(1, 0, c%on_ice_q_surf))
    if (c%on_ice_q_surf) then
      msg = nf90_put_att(id_file, nf90_global, 'climate_ice_q_surf__name', c%name_ice_q_surf)
      msg = nf90_put_att(id_file, nf90_global, 'climate_ice_q_surf_param__various', c%param_ice_q_surf)
    end if
    msg = nf90_put_att(id_file, nf90_global, 'climate_runoff_on__tf', merge(1, 0, c%on_runoff))
    if (c%on_runoff) then
      msg = nf90_put_att(id_file, nf90_global, 'climate_runoff__name', c%name_runoff)
      msg = nf90_put_att(id_file, nf90_global, 'climate_runoff_param__various', c%param_runoff)
    end if

    ! define dimensions
    msg = nf90_def_dim(id_file, 'x', s%nx, id_dim_x) 
    msg = nf90_def_dim(id_file, 'y', s%ny, id_dim_y) 
    msg = nf90_def_dim(id_file, 't', nf90_unlimited, id_dim_t)

    ! create coordinate variables
    msg = nf90_def_var(id_file, 't', rp_nc, id_dim_t, id_var)
    msg = nf90_put_att(id_file, id_var, 'long_name', 'model_time')
    msg = nf90_put_att(id_file, id_var, 'units', 'a')

    msg = nf90_def_var(id_file, 'x', rp_nc, id_dim_x, id_var)
    msg = nf90_put_att(id_file, id_var, 'long_name', 'x_coord')
    msg = nf90_put_att(id_file, id_var, 'units', 'm')
    
    msg = nf90_def_var(id_file, 'y', rp_nc, id_dim_y, id_var)
    msg = nf90_put_att(id_file, id_var, 'long_name', 'y_coord')
    msg = nf90_put_att(id_file, id_var, 'units', 'm')
    
    ! create variables
    if (io%write_topo) then
     	msg = nf90_def_var(id_file, 'topo', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'topography')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm')
    end if
    if (io%write_temp_surf) then
     	msg = nf90_def_var(id_file, 'temp_surf', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'surface_temperature')
     	msg = nf90_put_att(id_file, id_var, 'units', 'C')
    end if
    if (io%write_precip) then
     	msg = nf90_def_var(id_file, 'precip', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'precipitation_rate')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm_water/a')
    end if
    if (io%write_ice_q_surf) then
     	msg = nf90_def_var(id_file, 'ice_q_surf', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'surface_ice_flux')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm_ice/a')
    end if
    if (io%write_runoff) then
     	msg = nf90_def_var(id_file, 'runoff', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'runoff rate')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm_water/a')
    end if

    ! exit definition mode
    msg = nf90_enddef(id_file) 

    ! populate dimension variables
    msg = nf90_inq_varid(id_file, 'x', id_var)
    msg = nf90_put_var(id_file, id_var, s%x(2:s%nx+1))

    msg = nf90_inq_varid(id_file, 'y', id_var)
    msg = nf90_put_var(id_file, id_var, s%y(2:s%ny+1))

    ! close file
    msg = nf90_close(id_file)

  end subroutine create_output


  ! --------------------------------------------------------------------------
  ! SUB: print model status update to stdout
  ! --------------------------------------------------------------------------
  subroutine write_status(s)
    
    type(state_type), intent(in) :: s

    print "('MODEL TIME [a]           : ', EN11.3)", s%time_now 
    print "('TOPO (max, mean, min) [m]: ', EN11.3, ', ', EN11.3, ', ', EN11.3)", &
          maxval(s%topo), sum(s%topo)/size(s%topo), minval(s%topo)
    print *, ''

  end subroutine


  ! ---------------------------------------------------------------------------
  ! SUB: Write output step
  ! ---------------------------------------------------------------------------
  subroutine write_output_step(io, s)
    
    class(io_type), intent(inout) :: io
    type(state_type), intent(in) :: s

    integer :: i0, i1, j0, j1, msg, id_file, id_var

    ! increment step counter
    io%n_step = io%n_step+1

    ! define limits of interior points, for convenience
    i0 = 2; i1 = s%nx+1
    j0 = 2; j1 = s%ny+1

    ! open file
    msg = nf90_open(io%name_out, nf90_write, id_file)

    ! write time data
    msg = nf90_inq_varid(id_file, 't', id_var)
    msg = nf90_put_var(id_file, id_var, real(s%time_now, rp), [io%n_step] )

    ! write arrays 
    if (io%write_topo) then
      msg = nf90_inq_varid(id_file, 'topo', id_var)
      msg = nf90_put_var(id_file, id_var, real(s%topo(i0:i1, j0:j1), rp), [1, 1, io%n_step])
    end if
    if (io%write_temp_surf) then
      msg = nf90_inq_varid(id_file, 'temp_surf', id_var)
      msg = nf90_put_var(id_file, id_var, real(s%temp_surf(i0:i1, j0:j1), rp), [1, 1, io%n_step])
    end if
    if (io%write_precip) then
      msg = nf90_inq_varid(id_file, 'precip', id_var)
      msg = nf90_put_var(id_file, id_var, real(s%precip(i0:i1, j0:j1), rp), [1, 1, io%n_step])
    end if
    if (io%write_ice_q_surf) then
      msg = nf90_inq_varid(id_file, 'ice_q_surf', id_var)
      msg = nf90_put_var(id_file, id_var, real(s%ice_q_surf(i0:i1, j0:j1), rp), [1, 1, io%n_step])
    end if
    if (io%write_runoff) then
      msg = nf90_inq_varid(id_file, 'runoff', id_var)
      msg = nf90_put_var(id_file, id_var, real(s%runoff(i0:i1, j0:j1), rp), [1, 1, io%n_step])
    end if

    ! close file
    msg = nf90_close(id_file)

  end subroutine write_output_step

end module io_mod
