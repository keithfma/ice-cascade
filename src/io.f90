! =============================================================================
! Input/output variables and procedures routines for ice-cascade.
!
! Contains:
!   io_type: Public, object for io vars and methods
!   
! ============================================================================

module io_mod

use kinds_mod, only: rp, rp_nc
use common_mod, only: common_type
use time_mod, only: time_type
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
    integer :: n_step_out ! counter for current output step
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
  subroutine read_param(io, t, c, cl)

    class(io_type), intent(out) :: io
    type(time_type), intent(out) :: t
    type(common_type), intent(out) :: c
    type(climate_type), intent(out) :: cl

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
    read(55, *) t%start
    read(55, *) t%finish
    read(55, *) t%step
    read(55, *) c%rhoi
    read(55, *) c%nx	
    read(55, *) c%ny
    read(55, *) c%dx
    read(55, *) c%dy	
    read(55, *) io%name_topo 
    read(55, *) cl%on_temp_surf 
    read(55, *) cl%name_temp_surf 
    read(55, *) cl%param_temp_surf(:) 
    read(55, *) cl%on_precip 
    read(55, *) cl%name_precip
    read(55, *) cl%param_precip(:) 
    read(55, *) cl%on_ice_q_surf 
    read(55, *) cl%name_ice_q_surf 
    read(55, *) cl%param_ice_q_surf(:) 
    read(55, *) cl%on_runoff 
    read(55, *) cl%name_runoff 
    read(55, *) cl%param_runoff(:) 
    read(55, *) io%name_out	
    read(55, *) t%step_out
    read(55, *) io%write_topo
    read(55, *) io%write_temp_surf
    read(55, *) io%write_precip
    read(55, *) io%write_ice_q_surf
    read(55, *) io%write_runoff

  end subroutine read_param


  ! ---------------------------------------------------------------------------
  ! SUB: read the initial values for state variables
  ! TO-DO: add initial ice thickness
  ! ---------------------------------------------------------------------------
  subroutine read_initial_vals(io, c)

    class(io_type), intent(in) :: io 
    type(common_type), intent(inout) :: c

    ! initial topography
    select case(io%name_topo)
    case('zero')
      c%topo = 0.0_rp
    case default
      call read_array_nc(io%name_topo, c%nx, c%ny, c%topo)
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
  subroutine create_output(io, t, c)

    class(io_type), intent(in) :: io
    type(time_type), intent(in) :: t
    type(common_type), intent(in) :: c

    logical :: shuf
    integer :: fchunk(3), i, j, defLvl, msg, id_file, id_dim_x, id_dim_y, &
               id_dim_t, id_var

    ! define compression and chunking parameters 
    defLvl = 1 ! compression, 0 = none, 9 = max, best value is 1
    fchunk = [c%nx, c%ny, 1]
    shuf = .true.

    ! create new file
    msg = nf90_create(io%name_out, nf90_netcdf4, id_file)

    ! write parameters as global attributes
    msg = nf90_put_att(id_file, nf90_global, 'time_start_time__a', t%start)
    msg = nf90_put_att(id_file, nf90_global, 'time_finish_time__a', t%finish)
    msg = nf90_put_att(id_file, nf90_global, 'time_step__a', t%step)
    msg = nf90_put_att(id_file, nf90_global, 'time_step_output__a', t%step_out)
    msg = nf90_put_att(id_file, nf90_global, 'density_ice__kg1m-3', c%rhoi)
    msg = nf90_put_att(id_file, nf90_global, 'grid_nx__1', c%nx)
    msg = nf90_put_att(id_file, nf90_global, 'grid_ny__1', c%ny)
    msg = nf90_put_att(id_file, nf90_global, 'grid_dx__m', c%dx)
    msg = nf90_put_att(id_file, nf90_global, 'grid_dy__m', c%dy)
    msg = nf90_put_att(id_file, nf90_global, 'topo_initial__name', io%name_topo)

    ! define dimensions
    msg = nf90_def_dim(id_file, 'x', c%nx, id_dim_x) 
    msg = nf90_def_dim(id_file, 'y', c%ny, id_dim_y) 
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
    
    ! topography
    if (io%write_topo) then
     	msg = nf90_def_var(id_file, 'topo', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'topography')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm')
    end if

    ! exit definition mode
    msg = nf90_enddef(id_file) 

    ! populate dimension variables
    msg = nf90_inq_varid(id_file, 'x', id_var)
    msg = nf90_put_var(id_file, id_var, c%x(2:c%nx+1))

    msg = nf90_inq_varid(id_file, 'y', id_var)
    msg = nf90_put_var(id_file, id_var, c%y(2:c%ny+1))

    ! close file
    msg = nf90_close(id_file)

  end subroutine create_output


  ! --------------------------------------------------------------------------
  ! SUB: print model status update to stdout
  ! --------------------------------------------------------------------------
  subroutine write_status(t, c)
    
    type(time_type), intent(in) :: t
    type(common_type), intent(in) :: c

    print "('MODEL TIME [a]           : ', EN11.3)", t%now 
    print "('TOPO (max, mean, min) [m]: ', EN11.3, ', ', EN11.3, ', ', EN11.3)", maxval(c%topo), &
          sum(c%topo)/size(c%topo), minval(c%topo)
    print *, ''

  end subroutine


  ! ---------------------------------------------------------------------------
  ! SUB: Write output step
  ! ---------------------------------------------------------------------------
  subroutine write_output_step(io, t, c)
    
    class(io_type), intent(inout) :: io
    type(time_type), intent(in) :: t
    type(common_type), intent(in) :: c

    integer :: i0, i1, j0, j1, msg, id_file, id_var

    ! increment step counter
    io%n_step_out = io%n_step_out+1

    ! define limits of interior points, for convenience
    i0 = 2; i1 = c%nx+1
    j0 = 2; j1 = c%ny+1

    ! open file
    msg = nf90_open(io%name_out, nf90_write, id_file)

    ! write time data
    msg = nf90_inq_varid(id_file, 't', id_var)
    msg = nf90_put_var(id_file, id_var, real(t%now, rp), [io%n_step_out] )

    ! write topography data
    if (io%write_topo) then
      msg = nf90_inq_varid(id_file, 'topo', id_var)
      msg = nf90_put_var(id_file, id_var, real(c%topo(i0:i1, j0:j1), rp), [1, 1, io%n_step_out])
    end if

    ! close file
    msg = nf90_close(id_file)

  end subroutine write_output_step

end module io_mod
