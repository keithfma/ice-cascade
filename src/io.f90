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
use netcdf

implicit none
private
public :: io_type


  ! ---------------------------------------------------------------------------
  ! TYPE: input/output vars and procedures
  ! ---------------------------------------------------------------------------
  type io_type
    character(len=100) :: name_run ! user defined run name
    character(len=100) :: name_out ! output file name
    character(len=100) :: name_topo ! initial topography name
    logical :: write_topo ! flag 
  contains
    procedure, pass :: read_param ! read parameters from the input file
    procedure, pass :: init ! initialize object
    procedure, pass :: read_initial_vals ! read initial values 
    procedure, pass :: create_output ! create output file
  end type io_type


contains


  ! ---------------------------------------------------------------------------
  ! SUB: Read input parameters from file
  ! ---------------------------------------------------------------------------
  subroutine read_param(w, t, c)

    class(io_type), intent(out) :: w
    type(time_type), intent(out) :: t
    type(common_type), intent(out) :: c

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
    read(55, *) w%name_run	
    read(55, *) t%start
    read(55, *) t%finish
    read(55, *) t%step
    read(55, *) t%step_out
    read(55, *) c%rhoi
    read(55, *) c%nx	
    read(55, *) c%ny
    read(55, *) c%dx
    read(55, *) c%dy	
    read(55, *) w%name_topo 
    read(55, *) w%write_topo

  end subroutine read_param


  ! ---------------------------------------------------------------------------
  ! SUB: initialize object
  ! ---------------------------------------------------------------------------
  subroutine init(w)
    
    class(io_type), intent(inout) :: w

    w%name_out = trim(w%name_run)//'.out'
    
    ! disable writing for diabled components

  end subroutine init


  ! ---------------------------------------------------------------------------
  ! SUB: read the initial values for state variables
  ! TO-DO: add initial ice thickness
  ! ---------------------------------------------------------------------------
  subroutine read_initial_vals(w, c)

    class(io_type), intent(in) :: w
    type(common_type), intent(inout) :: c

    ! initial topography
    select case(w%name_topo)
    case('zero')
      c%topo = 0.0_rp
    case default
      call read_array_nc(w%name_topo, c%nx, c%ny, c%topo)
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
  subroutine create_output(w, t, c)

    class(io_type), intent(in) :: w
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
    msg = nf90_create(w%name_out, nf90_netcdf4, id_file)

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
    msg = nf90_put_att(id_file, nf90_global, 'topo_initial__name', w%name_topo)
!    ! climate
!    msg = nf90_put_att(id_file, nf90_global, 'climate_temp_on__tf', merge(1, 0, fclimate%on_t))
!    if (fclimate%on_t) then
!      msg = nf90_put_att(id_file, nf90_global, 'climate_temp__name', fclimate%tName)
!      msg = nf90_put_att(id_file, nf90_global, 'climate_temp_param__various', fclimate%tParam)
!    end if
!    msg = nf90_put_att(id_file, nf90_global, 'climate_precip_on__tf', merge(1, 0, fclimate%on_p))
!    if (fclimate%on_p) then
!      msg = nf90_put_att(id_file, nf90_global, 'climate_precip__name', fclimate%pName)
!      msg = nf90_put_att(id_file, nf90_global, 'climate_precip_param__various', fclimate%pParam)
!    end if
!    msg = nf90_put_att(id_file, nf90_global, 'climate_iceflux_on__tf', merge(1, 0, fclimate%on_i))
!    if (fclimate%on_i) then
!      msg = nf90_put_att(id_file, nf90_global, 'climate_iceflux__name', fclimate%iName)
!      msg = nf90_put_att(id_file, nf90_global, 'climate_iceflux_param__various', fclimate%iParam)
!    end if
!    ! ice
!    msg = nf90_put_att(id_file, nf90_global, 'ice_on__tf', merge(1, 0, fice%on))
!    if (fice%on) then
!      msg = nf90_put_att(id_file, nf90_global, 'ice_verbosity__flag',fice%verbose)
!      msg = nf90_put_att(id_file, nf90_global, 'ice_defm_coeff_prefactor__Pa-3a-1', fice%A0)
!      msg = nf90_put_att(id_file, nf90_global, 'ice_north_bc__name', trim(fice%nbcName))
!      msg = nf90_put_att(id_file, nf90_global, 'ice_south_bc__name', trim(fice%sbcName))
!      msg = nf90_put_att(id_file, nf90_global, 'ice_west_bc__name', trim(fice%wbcName))
!      msg = nf90_put_att(id_file, nf90_global, 'ice_east_bc__name', trim(fice%ebcName))
!      msg = nf90_put_att(id_file, nf90_global, 'ice_flow_method__name', trim(fice%flowName))
!      msg = nf90_put_att(id_file, nf90_global, 'ice_initial_thickness__name', trim(fice%h0Name))
!      msg = nf90_put_att(id_file, nf90_global, 'ice_exact_solution__name', trim(fice%solnName))
!    end if
!    ! hill
!    msg = nf90_put_att(id_file, nf90_global, 'hill_on__tf', merge(1, 0, fhill%on))
!    if (fhill%on) then
!      msg = nf90_put_att(id_file, nf90_global, 'hill_diffusivity__m2a-1', fhill%D)
!      msg = nf90_put_att(id_file, nf90_global, 'hill_north_bc__name', trim(fhill%nbcName))
!      msg = nf90_put_att(id_file, nf90_global, 'hill_south_bc__name', trim(fhill%sbcName))
!      msg = nf90_put_att(id_file, nf90_global, 'hill_west_bc__name', trim(fhill%wbcName))
!      msg = nf90_put_att(id_file, nf90_global, 'hill_east_bc__name', trim(fhill%ebcName))
!      msg = nf90_put_att(id_file, nf90_global, 'hill_topo_soln__name', trim(fhill%solnName))
!    end if

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
    if (w%write_topo) then
     	msg = nf90_def_var(id_file, 'topo', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'topography')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm')
    end if

!    ! create climate variables
!    if (fclimate%write_t) then
!     	msg = nf90_def_var(id_file, 'fclim_t', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'surface_temperature_high_res')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'C')
!    end if
!
!    if (fclimate%write_p) then
!     	msg = nf90_def_var(id_file, 'fclim_p', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'precipitation_rate_high_res')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm a-1')
!    end if
!
!    if (fclimate%write_i) then
!     	msg = nf90_def_var(id_file, 'fclim_i', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'surface_ice_flux_high_res')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm a-1')
!    end if
!
!    ! create ice variables
!    if (fice%write_h) then
!     	msg = nf90_def_var(id_file, 'fice_h', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_thickness_high_res')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm')
!    end if
!
!    if (fice%write_uvdefm) then
!     	msg = nf90_def_var(id_file, 'fice_udefm', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_deformation_velocity_xdir_high_res')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm a-1')
!
!     	msg = nf90_def_var(id_file, 'fice_vdefm', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_deformation_velocity_ydir_high_res')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm a-1')
!    end if
!
!    if (fice%write_soln) then
!     	msg = nf90_def_var(id_file, 'fice_soln_h', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_thickness_exact_solution_high_res')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm')
!    end if
!
!    ! create hillslope variables
!    if (fhill%write_dzdt) then
!     	msg = nf90_def_var(id_file, 'fhill_dzdt', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'hillslope_dzdt_high_res')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm a-1')
!    end if
!
!    if (fhill%write_soln) then
!     	msg = nf90_def_var(id_file, 'fhill_soln', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'hillslope_topo_solution_high_res')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm')
!    end if

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


end module io_mod
