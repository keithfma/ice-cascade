! =============================================================================
! Input/output procedures for ice-cascade.
!
! Description: The model reads from and writes to netCDF files with a specific
!   format. All the expected parameters MUST be defined as global parameters in
!   the input file. Missing parameters will cause the program to exit with an
!   error. All the state variables MAY be defined in the input file, in which
!   case they are read in as the initial values for these variables. Any issing
!   variables will be set to the default value (0). However, note that the
!   various model components are run at the initial time, and may overwrite
!   the supplied input data (e.g. if you provide an intitial temperature in the
!   temp_surf variable, but also select a climate model component that sets the
!   temp_surf variable, the climate procedure takes precedence).
!
! Public: read_param, read_var, write_file, write_step, write_status
!
! Private: get_var_1, get_var_2, req, opt, maxval_intr, minval_intr, meanval_intr
!   
! ============================================================================

module io

use kinds, only: rp, rp_nc
use param, only: param_type
use state, only: state_type
use netcdf
use ieee_arithmetic, only: ieee_is_nan

implicit none
private
public :: read_param, read_var, write_file, write_step, write_status

contains


  ! ---------------------------------------------------------------------------
  function get_var_1(netcdf_id, var_name, n, required) result (var_data)
  !
    integer, intent(in) :: netcdf_id ! handle for open netcdf file
    character(len=*), intent(in) :: var_name ! name of variable in netcdf file
    integer, intent(in) :: n ! length of output
    logical, intent(in) :: required ! variable is required (T) or optional (F)
    real(rp), allocatable :: var_data(:)
  !
  ! ABOUT: Read vector from netcdf file, handle read errors and null variables
  ! ---------------------------------------------------------------------------

    integer :: err, var_id

    ! allocate ouput variable
    allocate(var_data(n))

    ! find and read variable
    err = nf90_inq_varid(netcdf_id, trim(var_name), var_id)
    if (err .eq. nf90_noerr) err = nf90_get_var(netcdf_id, var_id, var_data)

    ! handle errors and missing vars
    if (err .ne. nf90_noerr) then
      print *, trim(nf90_strerror(err))
      if (required) then
        print *, 'required variable ('//trim(var_name)//') is not readable, stopping'
        stop 
      else
        print *, 'optional variable ('//trim(var_name)//') is not readable, set to 0.0'
        var_data = 0.0_rp
      end if
    end if

    ! handle null/dummy vars (all NAN indicates the var should not be read)
    if (all(ieee_is_nan(var_data))) then
      if (required) then
        print *, 'required variable ('//trim(var_name)//') is null, stopping'
        stop 
      else
        print *, 'optional variable ('//trim(var_name)//') is null, set to 0.0'
        var_data = 0.0_rp
      end if
    end if
      
    return

  end function get_var_1


  ! ---------------------------------------------------------------------------
  function get_var_2(netcdf_id, var_name, n1, n2, required) result (var_data)
  !
    integer, intent(in) :: netcdf_id ! handle for open netcdf file
    character(len=*), intent(in) :: var_name ! name of variable in netcdf file
    integer, intent(in) :: n1, n2 ! dimensions for output
    logical, intent(in) :: required ! variable is required (T) or optional (F)
    real(rp), allocatable :: var_data(:, :)
  !
  ! ABOUT: Read 2D grid from netcdf file, handle read errors and null variables
  ! ---------------------------------------------------------------------------

    integer :: err, var_id

    ! allocate ouput variable
    allocate(var_data(n1, n2))

    ! find and read variable
    err = nf90_inq_varid(netcdf_id, trim(var_name), var_id)
    if (err .eq. nf90_noerr) err = nf90_get_var(netcdf_id, var_id, var_data)

    ! handle errors and missing vars
    if (err .ne. nf90_noerr) then
      print *, trim(nf90_strerror(err))
      if (required) then
        print *, 'required variable ('//trim(var_name)//') is not readable, stopping'
        stop 
      else
        print *, 'optional variable ('//trim(var_name)//') is not readable, set to 0.0'
        var_data = 0.0_rp
      end if
    end if

    ! handle null/dummy vars (all NAN indicates the var should not be read)
    if (all(ieee_is_nan(var_data))) then
      if (required) then
        print *, 'required variable ('//trim(var_name)//') is null, stopping'
        stop 
      else
        print *, 'optional variable ('//trim(var_name)//') is null, set to 0.0'
        var_data = 0.0_rp
      end if
    end if
      
    return

  end function get_var_2


  ! ---------------------------------------------------------------------------
  subroutine req(err)
  ! 
    integer, intent (in) :: err
  !
  ! ABOUT: Error handling for required netcdf actions 
  !---------------------------------------------------------------------------

    if(err .ne. nf90_noerr) then
      print *, 'Fatal IO error: ', trim(nf90_strerror(err))
      stop 
    end if

  end subroutine req 


  ! ---------------------------------------------------------------------------
  subroutine opt(str, e)
  ! 
    character(len=*), intent(in) :: str
    integer, intent (in) :: e
  !
  ! ABOUT: Error handling for optional netcdf actions 
  !---------------------------------------------------------------------------

    if(e .ne. nf90_noerr) then
      print *, '(optional) ', str, trim(nf90_strerror(e))
    end if

  end subroutine opt


  ! ---------------------------------------------------------------------------
  function maxval_intr(array) result(val)
  !
    real(rp), intent(in) :: array(:,:)
    real(rp) :: val
  !
  ! ABOUT: max value of interior points of 2D array of type real(rp)
  ! ---------------------------------------------------------------------------

    integer :: n1, n2

    n1 = size(array,1)
    n2 = size(array,2)
    val = maxval(array(2:n1-1,2:n2-1))
    
  end function maxval_intr
  
  
  ! ---------------------------------------------------------------------------
  function minval_intr(array) result(val)
  !
    real(rp), intent(in) :: array(:,:)
    real(rp) :: val
  !
  ! ABOUT: min value of interior points of 2D array of type real(rp)
  ! ---------------------------------------------------------------------------

    integer :: n1, n2

    n1 = size(array,1)
    n2 = size(array,2)
    val = minval(array(2:n1-1,2:n2-1))
    
  end function minval_intr


  ! ---------------------------------------------------------------------------
  function meanval_intr(array) result(val)
  ! 
    real(rp), intent(in) :: array(:,:)
    real(rp) :: val 
  !
  ! ABOUT: mean value of interior points of 2D array of type real(rp)
  ! ---------------------------------------------------------------------------

    integer :: n1, n2

    n1 = size(array,1)
    n2 = size(array,2)
    val = sum(array(2:n1-1,2:n2-1))/((n1-2)*(n2-2))
    
  end function meanval_intr


  ! ---------------------------------------------------------------------------
  subroutine read_param(p)
  !
    type(param_type), intent(out) :: p
  !
  ! ABOUT: Read input parameters from file
  ! ---------------------------------------------------------------------------
    
    integer :: glb, ncid, n

    ! Get input arguments
    select case (command_argument_count())
      case (2)
      	call get_command_argument(1, p%input_file) 
      	call get_command_argument(2, p%output_file) 
      case default
      	print*,'ICE-CASCADE expects exactly 2 input arguments'
      	stop -1
    end select

    ! Define shorthand variables
    glb = nf90_global

    ! Open file
    call req(nf90_open(p%input_file, nf90_nowrite, ncid))

    ! Allocate vector attributes
    call req( nf90_inquire_attribute(ncid, glb, 'time_write__a', len = n) )
    allocate(p%time_write(n))
    call req( nf90_inquire_attribute(ncid, glb, 'climate_param__var', len = n) )
    allocate(p%climate_param(n))
    call req( nf90_inquire_attribute(ncid, glb, 'ice_param__var', len = n) )
    allocate(p%ice_param(n))
    call req( nf90_inquire_attribute(ncid, glb, 'ice_soln_param__var', len = n) )
    allocate(p%ice_soln_param(n))

    ! Allocate vector attributes

    ! Read parameters from global attributes
    call req(nf90_get_att(ncid, glb, 'nx__1', p%nx))
    call req(nf90_get_att(ncid, glb, 'ny__1', p%ny))
    call req(nf90_get_att(ncid, glb, 'lx__m', p%lx))
    call req(nf90_get_att(ncid, glb, 'ly__m', p%ly))
    call req(nf90_get_att(ncid, glb, 'dx__m', p%dx))
    call req(nf90_get_att(ncid, glb, 'dy__m', p%dy))
    call req(nf90_get_att(ncid, glb, 'rhoi__kg_m3', p%rhoi))
    call req(nf90_get_att(ncid, glb, 'grav__m_s2', p%grav))
    call req(nf90_get_att(ncid, glb, 'time_start__a', p%time_start))
    call req(nf90_get_att(ncid, glb, 'time_finish__a', p%time_finish))
    call req(nf90_get_att(ncid, glb, 'time_step__a', p%time_step))
    call req(nf90_get_att(ncid, glb, 'time_write__a', p%time_write))
    call req(nf90_get_att(ncid, glb, 'climate_name', p%climate_name))
    call req(nf90_get_att(ncid, glb, 'climate_param__var', p%climate_param))
    call req(nf90_get_att(ncid, glb, 'ice_name', p%ice_name))
    call req(nf90_get_att(ncid, glb, 'ice_param__var', p%ice_param))
    call req(nf90_get_att(ncid, glb, 'ice_bc_name__nesw', p%ice_bc_name))
    call req(nf90_get_att(ncid, glb, 'ice_soln_name', p%ice_soln_name))
    call req(nf90_get_att(ncid, glb, 'ice_soln_param__var', p%ice_soln_param))
    call req(nf90_get_att(ncid, glb, 'write_topo', p%write_topo))
    call req(nf90_get_att(ncid, glb, 'write_topo_dot_ice', p%write_topo_dot_ice ));
    call req(nf90_get_att(ncid, glb, 'write_surf', p%write_surf));
    call req(nf90_get_att(ncid, glb, 'write_temp_surf', p%write_temp_surf));
    call req(nf90_get_att(ncid, glb, 'write_temp_ice', p%write_temp_ice));
    call req(nf90_get_att(ncid, glb, 'write_temp_base', p%write_temp_base));
    call req(nf90_get_att(ncid, glb, 'write_precip', p%write_precip));
    call req(nf90_get_att(ncid, glb, 'write_runoff', p%write_runoff));
    call req(nf90_get_att(ncid, glb, 'write_ice_q_surf', p%write_ice_q_surf));
    call req(nf90_get_att(ncid, glb, 'write_ice_h', p%write_ice_h));
    call req(nf90_get_att(ncid, glb, 'write_ice_h_dot', p%write_ice_h_dot));
    call req(nf90_get_att(ncid, glb, 'write_ice_uv_defm', p%write_ice_uv_defm));
    call req(nf90_get_att(ncid, glb, 'write_ice_uv_slid', p%write_ice_uv_slid));
    call req(nf90_get_att(ncid, glb, 'write_ice_h_soln', p%write_ice_h_soln));

    ! Close file
    call req(nf90_close(ncid))

  end subroutine read_param


  !----------------------------------------------------------------------------
  subroutine read_var(p, s)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: Read initial variables from netcdf filei
  !
  !   Unspecified variables retain the default value (0) State variable arrays
  !   include ghost points at boundaries, while input arrays do not.
  ! ---------------------------------------------------------------------------

    integer :: e, ncid, varid
    real(rp), allocatable :: buffer(:,:)
   
    ! Allocate read buffer for interior points only
    allocate(buffer(p%nx-2, p%ny-2))

    ! Open file
    call req(nf90_open(p%input_file, nf90_nowrite, ncid))

    ! Read variables
    s%x(2:p%nx-1) = get_var_1(ncid, 'x', p%nx-2, .true.) 

    s%y(2:p%ny-1) = get_var_1(ncid, 'y', p%ny-2, .true.) 

    s%topo(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'topo', p%nx-2, p%ny-2, .false.)

    s%topo_dot_ice(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'topo_dot_ice', p%nx-2, p%ny-2, .false.)

    s%surf(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'surf', p%nx-2, p%ny-2, .false.)

    s%temp_surf(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'temp_surf', p%nx-2, p%ny-2, .false.)

    s%temp_ice(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'temp_ice', p%nx-2, p%ny-2, .false.)

    s%temp_base(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'temp_base', p%nx-2, p%ny-2, .false.)

    s%precip(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'precip', p%nx-2, p%ny-2, .false.)

    s%runoff(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'runoff', p%nx-2, p%ny-2, .false.)

    s%ice_q_surf(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'ice_q_surf', p%nx-2, p%ny-2, .false.)

    s%ice_h(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'ice_h', p%nx-2, p%ny-2, .false.)

    s%ice_h_dot(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'ice_h_dot', p%nx-2, p%ny-2, .false.)

    s%ice_h_soln(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'ice_h_soln', p%nx-2, p%ny-2, .false.)

    s%ice_u_defm(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'ice_u_defm', p%nx-2, p%ny-2, .false.)

    s%ice_v_defm(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'ice_v_defm', p%nx-2, p%ny-2, .false.)

    s%ice_u_slid(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'ice_u_slid', p%nx-2, p%ny-2, .false.)

    s%ice_v_slid(2:p%nx-1,2:p%ny-1) = &
      get_var_2(ncid, 'ice_v_slid', p%nx-2, p%ny-2, .false.)

    ! Close file
    call req(nf90_close(ncid))

  end subroutine read_var


  ! ---------------------------------------------------------------------------
  subroutine write_file(p, s)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s
  !
  ! ABOUT: Create output netcdf file
  !
  !   Note that state variables include ghost points at grid boundaries, but
  !   output grids do not. 
  ! ---------------------------------------------------------------------------

    logical :: shuf
    integer :: chunk(3), deflate, e, ncid, tf, tid, vid, xid, yid

    ! define compression and chunking parameters 
    chunk = [p%nx-2, p%ny-2, 1] ! x, y, t
    deflate = 1 ! compression, 0 = none, 9 = max, best value is 1
    shuf = .true.

    ! create new file
    call req(nf90_create(p%output_file, nf90_netcdf4, ncid))

    ! write parameters as global attributes
    e = nf90_put_att(ncid, nf90_global, 'nx__1', p%nx-2)
    e = nf90_put_att(ncid, nf90_global, 'ny__1', p%ny-2)
    e = nf90_put_att(ncid, nf90_global, 'lx__m', p%lx)
    e = nf90_put_att(ncid, nf90_global, 'ly__m', p%ly)
    e = nf90_put_att(ncid, nf90_global, 'dx__m', p%dx)
    e = nf90_put_att(ncid, nf90_global, 'dy__m', p%dy)
    e = nf90_put_att(ncid, nf90_global, 'rhoi__kg_m3', p%rhoi)
    e = nf90_put_att(ncid, nf90_global, 'grav__m_s2', p%grav)
    e = nf90_put_att(ncid, nf90_global, 'time_start__a', p%time_start )
    e = nf90_put_att(ncid, nf90_global, 'time_finish__a', p%time_finish)
    e = nf90_put_att(ncid, nf90_global, 'time_step__a', p%time_step)
    e = nf90_put_att(ncid, nf90_global, 'time_write__a', p%time_write)
    e = nf90_put_att(ncid, nf90_global, 'climate_name', p%climate_name)
    e = nf90_put_att(ncid, nf90_global, 'climate_param__var', p%climate_param)
    e = nf90_put_att(ncid, nf90_global, 'ice_name', p%ice_name)
    e = nf90_put_att(ncid, nf90_global, 'ice_param__var', p%ice_param)
    e = nf90_put_att(ncid, nf90_global, 'ice_bc_name__nesw', p%ice_bc_name)
    e = nf90_put_att(ncid, nf90_global, 'ice_soln_name', p%ice_soln_name)
    e = nf90_put_att(ncid, nf90_global, 'ice_soln_param__var', p%ice_soln_param)
    e = nf90_put_att(ncid, nf90_global, 'write_topo', p%write_topo)
    e = nf90_put_att(ncid, nf90_global, 'write_topo_dot_ice', p%write_topo_dot_ice)
    e = nf90_put_att(ncid, nf90_global, 'write_surf', p%write_surf)
    e = nf90_put_att(ncid, nf90_global, 'write_temp_surf', p%write_temp_surf)
    e = nf90_put_att(ncid, nf90_global, 'write_temp_ice', p%write_temp_ice)
    e = nf90_put_att(ncid, nf90_global, 'write_temp_base', p%write_temp_base)
    e = nf90_put_att(ncid, nf90_global, 'write_precip', p%write_precip)
    e = nf90_put_att(ncid, nf90_global, 'write_runoff', p%write_runoff)
    e = nf90_put_att(ncid, nf90_global, 'write_ice_q_surf', p%write_ice_q_surf)
    e = nf90_put_att(ncid, nf90_global, 'write_ice_h', p%write_ice_h)
    e = nf90_put_att(ncid, nf90_global, 'write_ice_h_dot', p%write_ice_h_dot)
    e = nf90_put_att(ncid, nf90_global, 'write_ice_uv_defm', p%write_ice_uv_defm)
    e = nf90_put_att(ncid, nf90_global, 'write_ice_uv_slid', p%write_ice_uv_slid)
    e = nf90_put_att(ncid, nf90_global, 'write_ice_h_soln', p%write_ice_h_soln)

    ! define dimensions
    e = nf90_def_dim(ncid, 'x', p%nx-2, xid) 
    e = nf90_def_dim(ncid, 'y', p%ny-2, yid) 
    e = nf90_def_dim(ncid, 't', nf90_unlimited, tid)

    ! define variables
    e = nf90_def_var(ncid, 'x', rp_nc, xid, vid)
    e = nf90_put_att(ncid, vid, 'long_name', 'x_coord')
    e = nf90_put_att(ncid, vid, 'units', 'm')
    
    e = nf90_def_var(ncid, 'y', rp_nc, yid, vid)
    e = nf90_put_att(ncid, vid, 'long_name', 'y_coord')
    e = nf90_put_att(ncid, vid, 'units', 'm')

    e = nf90_def_var(ncid, 't', rp_nc, tid, vid)
    e = nf90_put_att(ncid, vid, 'long_name', 'model_time')
    e = nf90_put_att(ncid, vid, 'units', 'a')
    
    if (p%write_topo .eq. 1) then
     	e = nf90_def_var(ncid, 'topo', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate)
     	e = nf90_put_att(ncid, vid, 'long_name', 'topography')
     	e = nf90_put_att(ncid, vid, 'units', 'm')
    end if

    if (p%write_topo_dot_ice .eq. 1) then
     	e = nf90_def_var(ncid, 'topo_dot_ice', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'topo_rate_of_change_from_ice')
     	e = nf90_put_att(ncid, vid, 'units', 'm_a')
    end if

    if (p%write_surf .eq. 1) then
     	e = nf90_def_var(ncid, 'surf', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate)
     	e = nf90_put_att(ncid, vid, 'long_name', 'surface_elevation_including_ice')
     	e = nf90_put_att(ncid, vid, 'units', 'm')
    end if

    if (p%write_temp_surf .eq. 1) then
     	e = nf90_def_var(ncid, 'temp_surf', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'surface_temperature')
     	e = nf90_put_att(ncid, vid, 'units', 'C')
    end if

    if (p%write_temp_base .eq. 1) then
     	e = nf90_def_var(ncid, 'temp_base', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_basal_temperature')
     	e = nf90_put_att(ncid, vid, 'units', 'C')
    end if

    if (p%write_temp_ice .eq. 1) then
     	e = nf90_def_var(ncid, 'temp_ice', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_mean_temperature')
     	e = nf90_put_att(ncid, vid, 'units', 'C')
    end if

    if (p%write_precip .eq. 1) then
     	e = nf90_def_var(ncid, 'precip', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'precipitation_rate')
     	e = nf90_put_att(ncid, vid, 'units', 'mwater_a')
    end if

    if (p%write_runoff .eq. 1) then
     	e = nf90_def_var(ncid, 'runoff', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'runoff rate')
     	e = nf90_put_att(ncid, vid, 'units', 'mwater_a')
    end if

    if (p%write_ice_q_surf .eq. 1) then
     	e = nf90_def_var(ncid, 'ice_q_surf', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'surface_ice_flux')
     	e = nf90_put_att(ncid, vid, 'units', 'mice_a')
    end if

    if (p%write_ice_h .eq. 1) then
     	e = nf90_def_var(ncid, 'ice_h', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_thickness')
     	e = nf90_put_att(ncid, vid, 'units', 'm')
    end if

    if (p%write_ice_h_dot .eq. 1) then
     	e = nf90_def_var(ncid, 'ice_h_dot', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_thickness_rate_of_change')
     	e = nf90_put_att(ncid, vid, 'units', 'm_a')
    end if

    if (p%write_ice_uv_defm .eq. 1) then
     	e = nf90_def_var(ncid, 'ice_ud', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_deformation_velocity_x')
     	e = nf90_put_att(ncid, vid, 'units', 'm_a')

     	e = nf90_def_var(ncid, 'ice_vd', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_deformation_velocity_y')
     	e = nf90_put_att(ncid, vid, 'units', 'm_a')
    end if

    if (p%write_ice_uv_slid .eq. 1) then
     	e = nf90_def_var(ncid, 'ice_us', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_sliding_velocity_x')
     	e = nf90_put_att(ncid, vid, 'units', 'm_a')

     	e = nf90_def_var(ncid, 'ice_vs', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_sliding_velocity_y')
     	e = nf90_put_att(ncid, vid, 'units', 'm_a')
    end if

    if (p%write_ice_h_soln .eq. 1) then
     	e = nf90_def_var(ncid, 'ice_h_soln', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_thickness_solution')
     	e = nf90_put_att(ncid, vid, 'units', 'm')
    end if

    ! populate coordinate variables
    call req(nf90_enddef(ncid))

    call req(nf90_inq_varid(ncid, 'x', vid))
    call req(nf90_put_var(ncid, vid, s%x(2:p%nx-1)))

    call req(nf90_inq_varid(ncid, 'y', vid))
    call req(nf90_put_var(ncid, vid, s%y(2:p%ny-1)))

    ! close file
    call req(nf90_close(ncid))

  end subroutine write_file

  ! ---------------------------------------------------------------------------
  subroutine write_step(p, s)
  !  
    type(param_type), intent(inout) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: Append model state to output netcdf
  ! ---------------------------------------------------------------------------

    integer :: e, n, ncid, tid, vid

    ! open file
    e = nf90_open(p%output_file, nf90_write, ncid)

    ! get current step
    e = nf90_inq_dimid(ncid, 't', tid)
    e = nf90_inquire_dimension(ncid, tid, len = n)
    n = n+1

    ! write data
    e = nf90_inq_varid(ncid, 't', vid)
    e = nf90_put_var(ncid, vid, s%now, [n] )

    if (p%write_topo .eq. 1) then
      e = nf90_inq_varid(ncid, 'topo', vid)
      e = nf90_put_var(ncid, vid, s%topo(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if

    if (p%write_topo_dot_ice .eq. 1) then
      e = nf90_inq_varid(ncid, 'topo_dot_ice', vid)
      e = nf90_put_var(ncid, vid, s%topo_dot_ice(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_surf .eq. 1) then
      e = nf90_inq_varid(ncid, 'surf', vid)
      e = nf90_put_var(ncid, vid, s%surf(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if

    if (p%write_temp_surf .eq. 1) then
      e = nf90_inq_varid(ncid, 'temp_surf', vid)
      e = nf90_put_var(ncid, vid, s%temp_surf(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_temp_ice .eq. 1) then
      e = nf90_inq_varid(ncid, 'temp_ice', vid)
      e = nf90_put_var(ncid, vid, s%temp_ice(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_temp_base .eq. 1) then
      e = nf90_inq_varid(ncid, 'temp_base', vid)
      e = nf90_put_var(ncid, vid, s%temp_base(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_precip .eq. 1) then
      e = nf90_inq_varid(ncid, 'precip', vid)
      e = nf90_put_var(ncid, vid, s%precip(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_runoff .eq. 1) then
      e = nf90_inq_varid(ncid, 'runoff', vid)
      e = nf90_put_var(ncid, vid, s%runoff(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_ice_q_surf .eq. 1) then
      e = nf90_inq_varid(ncid, 'ice_q_surf', vid)
      e = nf90_put_var(ncid, vid, s%ice_q_surf(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_ice_h .eq. 1) then
      e = nf90_inq_varid(ncid, 'ice_h', vid)
      e = nf90_put_var(ncid, vid, s%ice_h(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_ice_h_dot .eq. 1) then
      e = nf90_inq_varid(ncid, 'ice_h_dot', vid)
      e = nf90_put_var(ncid, vid, s%ice_h_dot(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_ice_h_soln .eq. 1) then
      e = nf90_inq_varid(ncid, 'ice_h_soln', vid)
      e = nf90_put_var(ncid, vid, s%ice_h_soln(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_ice_uv_defm .eq. 1) then
      e = nf90_inq_varid(ncid, 'ice_u_defm', vid)
      e = nf90_put_var(ncid, vid, s%ice_u_defm(2:p%nx-1,2:p%ny-1), [1, 1, n])
      
      e = nf90_inq_varid(ncid, 'ice_v_defm', vid)
      e = nf90_put_var(ncid, vid, s%ice_v_defm(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_ice_uv_slid .eq. 1) then
      e = nf90_inq_varid(ncid, 'ice_u_slid', vid)
      e = nf90_put_var(ncid, vid, s%ice_u_slid(2:p%nx-1,2:p%ny-1), [1, 1, n])
    
      e = nf90_inq_varid(ncid, 'ice_v_slid', vid)
      e = nf90_put_var(ncid, vid, s%ice_v_slid(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if

    ! close file
    e = nf90_close(ncid)

  end subroutine write_step

  ! --------------------------------------------------------------------------
  subroutine write_status(p, s)
  ! 
    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s
  !
  ! ABOUT: print model status update to stdout
  ! --------------------------------------------------------------------------

      print "('TIME [a]                         : ', EN12.3)", s%now 

    if (p%write_topo .eq. 1) then
      print "('TOPO (max, mean, min) [m]        : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%topo), &
        meanval_intr(s%topo), &
        minval_intr(s%topo)
    end if

    if (p%write_topo_dot_ice .eq. 1) then
      print "('TOPO_DOT_ICE (max, mean, min) [m]: ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%topo_dot_ice), &
        meanval_intr(s%topo_dot_ice), &
        minval_intr(s%topo_dot_ice)
    end if

    if (p%write_surf .eq. 1) then
      print "('SURF (max, mean, min) [m]        : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%surf), &
        meanval_intr(s%surf), &
        minval_intr(s%surf)
    end if

    if (p%write_temp_surf .eq. 1) then
      print "('TEMP_SURF (max, mean, min) [C]   : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%temp_surf), &
        meanval_intr(s%temp_surf), &
        minval_intr(s%temp_surf)
    end if

    if (p%write_temp_base .eq. 1) then
      print "('TEMP_BASE (max, mean, min) [C]   : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%temp_base), &
        meanval_intr(s%temp_base), &
        minval_intr(s%temp_base)
    end if

    if (p%write_temp_ice .eq. 1) then
      print "('TEMP_ICE (max, mean, min) [C]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%temp_ice), &
        meanval_intr(s%temp_ice), &
        minval_intr(s%temp_ice)
    end if

    if (p%write_precip .eq. 1) then
      print "('PRECIP (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%precip), &
        meanval_intr(s%precip), &
        minval_intr(s%precip)
    end if

    if (p%write_runoff .eq. 1) then
      print "('RUNOFF (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%runoff), &
        meanval_intr(s%runoff), &
        minval_intr(s%runoff)
    end if

    if (p%write_ice_q_surf .eq. 1) then
      print "('ICE_Q_SURF (max, mean, min) [m/a]: ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_q_surf), &
        meanval_intr(s%ice_q_surf), &
        minval_intr(s%ice_q_surf)
    end if
    
    if (p%write_ice_h .eq. 1) then
      print "('ICE_H (max, mean, min) [m]       : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_h), &
        meanval_intr(s%ice_h), &
        minval_intr(s%ice_h)
    end if

    if (p%write_ice_h_dot .eq. 1) then
      print "('ICE_H_DOT (max, mean, min) [m/a] : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_h_dot), &
        meanval_intr(s%ice_h_dot), &
        minval_intr(s%ice_h_dot)
    end if

    if (p%write_ice_uv_defm .eq. 1) then
      print "('ICE_U_DEFM (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_u_defm), &
        meanval_intr(s%ice_u_defm)/size(s%ice_u_defm), &
        minval_intr(s%ice_u_defm)
      print "('ICE_V_DEFM (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_v_defm), &
        meanval_intr(s%ice_v_defm), &
        minval_intr(s%ice_v_defm)
    end if

    if (p%write_ice_uv_slid .eq. 1) then
      print "('ICE_U_SLID (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_u_slid), &
        meanval_intr(s%ice_u_slid), &
        minval_intr(s%ice_u_slid)
      print "('ICE_V_SLID (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_v_slid), &
        meanval_intr(s%ice_v_slid), &
        minval_intr(s%ice_v_slid)
    end if

    if (p%write_ice_h_soln .eq. 1) then
      print "('ICE_H_SOLN (max, mean, min) [m]  : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_h_soln), &
        meanval_intr(s%ice_h_soln), &
        minval_intr(s%ice_h_soln)
    end if

    print *, ''

  end subroutine write_status

end module io
