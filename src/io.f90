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
! Private: req, opt, maxval_intr, minval_intr, meanval_intr
!   
! ============================================================================

module io

use kinds, only: rp, rp_nc
use param, only: param_type
use state, only: state_type
use netcdf

implicit none
private
public :: read_param, read_var, write_file, write_step, write_status

contains


  ! ---------------------------------------------------------------------------
  subroutine req(str, e)
  ! 
    character(len=*), intent(in) :: str
    integer, intent (in) :: e
  !
  ! ABOUT: Error handling for required netcdf actions 
  !---------------------------------------------------------------------------

    if(e .ne. nf90_noerr) then
      print *, str, trim(nf90_strerror(e))
      stop 'Stopped'
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
    
    integer :: ncid, e, n, tf

    ! Get input arguments
    select case (command_argument_count())
      case (2)
      	call get_command_argument(1, p%input_file) 
      	call get_command_argument(2, p%output_file) 
      case default
      	print*,'ICE-CASCADE expects exactly 2 input arguments'
      	stop -1
    end select

    ! Open file
    e = nf90_open(p%input_file, nf90_nowrite, ncid)
    call req('read_param: open file: ', e)

    ! Read parameters from global attributes
    e = nf90_get_att(ncid, nf90_global, 'nx__1', p%nx)
    call req('read_param: nx__1: ', e)

    e = nf90_get_att(ncid, nf90_global, 'ny__1', p%ny)
    call req('read_param: ny__1: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'lx__m', p%lx)
    call req('read_param: lx__m: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'ly__m', p%ly)
    call req('read_param: ly__m: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'dx__m', p%dx)
    call req('read_param: dx__m: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'dy__m', p%dy)
    call req('read_param: dy__m: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'rhoi__kg_m3', p%rhoi)
    call req('read_param: rhoi__kg_m3: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'grav__m_s2', p%grav)
    call req('read_param: grav__m_s2: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'time_start__a', p%time_start )
    call req('read_param: time_start__a: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'time_finish__a', p%time_finish)
    call req('read_param: time_finish__a: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'time_step__a', p%time_step)
    call req('read_param: time_step__a: ', e)
    
    e = nf90_inquire_attribute(ncid, nf90_global, 'time_write__a', len = n)
    call req('read_param: time_write__a: ', e)
    allocate(p%time_write(n))
    e = nf90_get_att(ncid, nf90_global, 'time_write__a', p%time_write)
    call req('read_param: time_write__a: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'climate_name', p%climate_name)
    call req('read_param: climate_name: ', e)
    
    e = nf90_inquire_attribute(ncid, nf90_global, 'climate_param__var', len = n)
    call req('read_param: climate_param__var: ', e)
    allocate(p%climate_param(n))
    e = nf90_get_att(ncid, nf90_global, 'climate_param__var', p%climate_param)
    call req('read_param: climate_param__var: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'ice_name', p%ice_name)
    call req('read_param: ice_name: ', e)
    
    e = nf90_inquire_attribute(ncid, nf90_global, 'ice_param__var', len = n)
    call req('read_param: ice_param__var: ', e)
    allocate(p%ice_param(n))
    e = nf90_get_att(ncid, nf90_global, 'ice_param__var', p%ice_param)
    call req('read_param: ice_param__var: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'ice_bc_name__nesw', p%ice_bc_name)
    call req('read_param: ice_bc_name__nesw: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'ice_soln_name', p%ice_soln_name)
    call req('read_param: ice_soln_name: ', e)
    
    e = nf90_inquire_attribute(ncid, nf90_global, 'ice_soln_param__var', len = n)
    call req('read_param: ice_soln_param__var: ', e)
    allocate(p%ice_soln_param(n))
    e = nf90_get_att(ncid, nf90_global, 'ice_soln_param__var', p%ice_soln_param)
    call req('read_param: ice_soln_param__var: ', e)

    e = nf90_get_att(ncid, nf90_global, 'write_topo', tf)
    call req('read_param: write_topo: ', e)
    p%write_topo = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_topo_dot_ice', tf)
    call req('read_param: write_topo_dot_ice: ', e)
    p%write_topo_dot_ice = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_surf', tf)
    call req('read_param: write_surf: ', e)
    p%write_surf = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_temp_surf', tf)
    call req('read_param: write_temp_surf: ', e)
    p%write_temp_surf = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_temp_ice', tf)
    call req('read_param: write_temp_ice: ', e)
    p%write_temp_ice = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_temp_base', tf)
    call req('read_param: write_temp_base: ', e)
    p%write_temp_base = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_precip', tf)
    call req('read_param: write_precip: ', e)
    p%write_precip = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_runoff', tf)
    call req('read_param: write_runoff: ', e)
    p%write_runoff = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_ice_q_surf', tf)
    call req('read_param: write_ice_q_surf: ', e)
    p%write_ice_q_surf = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_ice_h', tf)
    call req('read_param: write_ice_h: ', e)
    p%write_ice_h = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_ice_h_dot', tf)
    call req('read_param: write_ice_h_dot: ', e)
    p%write_ice_h_dot = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_ice_uvd', tf)
    call req('read_param: write_ice_uvd: ', e)
    p%write_ice_uvd = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_ice_uvs', tf)
    call req('read_param: write_ice_uvs: ', e)
    p%write_ice_uvs = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_ice_h_soln', tf)
    call req('read_param: write_ice_h_soln: ', e)
    p%write_ice_h_soln = (tf .eq. 1)

    ! Close file
    e = nf90_close(ncid)
    call req('read_param: close file: ', e)

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
    
    ! Open file
    e = nf90_open(p%input_file, nf90_nowrite, ncid)
    call req('read vars: open file: ', e)

    ! Read variables
    e = nf90_inq_varid(ncid, 'x', varid)
    call req('read_vars: x: ', e)
    e = nf90_get_var(ncid, varid, s%x(2:p%nx-1))
    call req('read_vars: x: ', e)

    e = nf90_inq_varid(ncid, 'y', varid)
    call req('read_vars: y: ', e)
    e = nf90_get_var(ncid, varid, s%y(2:p%ny-1))
    call req('read_vars: y: ', e)

    e = nf90_inq_varid(ncid, 'topo', varid)
    call opt('read_vars: topo: ', e)
    e = nf90_get_var(ncid, varid, s%topo(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: topo: ', e)

    e = nf90_inq_varid(ncid, 'topo_dot_ice', varid)
    call opt('read_vars: topo_dot_ice: ', e)
    e = nf90_get_var(ncid, varid, s%topo_dot_ice(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: topo_dot_ice: ', e)

    e = nf90_inq_varid(ncid, 'surf', varid)
    call opt('read_vars: surf: ', e)
    e = nf90_get_var(ncid, varid, s%surf(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: surf: ', e)

    e = nf90_inq_varid(ncid, 'temp_surf', varid)
    call opt('read_vars: temp_surf: ', e)
    e = nf90_get_var(ncid, varid, s%temp_surf(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: temp_surf: ', e)

    e = nf90_inq_varid(ncid, 'temp_ice', varid)
    call opt('read_vars: temp_ice: ', e)
    e = nf90_get_var(ncid, varid, s%temp_ice(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: temp_ice: ', e)
    
    e = nf90_inq_varid(ncid, 'temp_base', varid)
    call opt('read_vars: temp_base: ', e)
    e = nf90_get_var(ncid, varid, s%temp_base(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: temp_base: ', e)

    e = nf90_inq_varid(ncid, 'precip', varid)
    call opt('read_vars: precip: ', e)
    e = nf90_get_var(ncid, varid, s%precip(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: precip: ', e)

    e = nf90_inq_varid(ncid, 'runoff', varid)
    call opt('read_vars: runoff: ', e)
    e = nf90_get_var(ncid, varid, s%runoff(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: runoff: ', e)

    e = nf90_inq_varid(ncid, 'ice_q_surf', varid)
    call opt('read_vars: ice_q_surf: ', e)
    e = nf90_get_var(ncid, varid, s%ice_q_surf(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: ice_q_surf: ', e)

    e = nf90_inq_varid(ncid, 'ice_h', varid)
    call opt('read_vars: ice_h: ', e)
    e = nf90_get_var(ncid, varid, s%ice_h(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: ice_h: ', e)

    e = nf90_inq_varid(ncid, 'ice_h_dot', varid)
    call opt('read_vars: ice_h_dot: ', e)
    e = nf90_get_var(ncid, varid, s%ice_h_dot(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: ice_h_dot: ', e)

    e = nf90_inq_varid(ncid, 'ice_h_soln', varid)
    call opt('read_vars: ice_h_soln: ', e)
    e = nf90_get_var(ncid, varid, s%ice_h_soln(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: ice_h_soln: ', e)

    e = nf90_inq_varid(ncid, 'ice_ud', varid)
    call opt('read_vars: ice_ud: ', e)
    e = nf90_get_var(ncid, varid, s%ice_ud(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: ice_ud: ', e)

    e = nf90_inq_varid(ncid, 'ice_vd', varid)
    call opt('read_vars: ice_vd: ', e)
    e = nf90_get_var(ncid, varid, s%ice_vd(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: ice_vd: ', e)

    e = nf90_inq_varid(ncid, 'ice_us', varid)
    call opt('read_vars: ice_us: ', e)
    e = nf90_get_var(ncid, varid, s%ice_us(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: ice_us: ', e)

    e = nf90_inq_varid(ncid, 'ice_vs', varid)
    call opt('read_vars: ice_vs: ', e)
    e = nf90_get_var(ncid, varid, s%ice_vs(2:p%nx-1,2:p%ny-1))
    call opt('read_vars: ice_vs: ', e)

    ! Close file
    e = nf90_close(ncid)
    call req('read_param: close file: ', e)

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
    e = nf90_create(p%output_file, nf90_netcdf4, ncid)
    call req('write_file: create file: ', e)

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

    tf = merge(1, 0, p%write_topo)
    e = nf90_put_att(ncid, nf90_global, 'write_topo', tf)

    tf = merge(1, 0, p%write_topo_dot_ice)
    e = nf90_put_att(ncid, nf90_global, 'write_topo_dot_ice', tf)

    tf = merge(1, 0, p%write_surf)
    e = nf90_put_att(ncid, nf90_global, 'write_surf', tf)

    tf = merge(1, 0, p%write_temp_surf)
    e = nf90_put_att(ncid, nf90_global, 'write_temp_surf', tf)

    tf = merge(1, 0, p%write_temp_ice)
    e = nf90_put_att(ncid, nf90_global, 'write_temp_ice', tf)

    tf = merge(1, 0, p%write_temp_base)
    e = nf90_put_att(ncid, nf90_global, 'write_temp_base', tf)

    tf = merge(1, 0, p%write_precip)
    e = nf90_put_att(ncid, nf90_global, 'write_precip', tf)

    tf = merge(1, 0, p%write_runoff)
    e = nf90_put_att(ncid, nf90_global, 'write_runoff', tf)

    tf = merge(1, 0, p%write_ice_q_surf)
    e = nf90_put_att(ncid, nf90_global, 'write_ice_q_surf', tf)

    tf = merge(1, 0, p%write_ice_h)
    e = nf90_put_att(ncid, nf90_global, 'write_ice_h', tf)

    tf = merge(1, 0, p%write_ice_h_dot)
    e = nf90_put_att(ncid, nf90_global, 'write_ice_h_dot', tf)

    tf = merge(1, 0, p%write_ice_uvd)
    e = nf90_put_att(ncid, nf90_global, 'write_ice_uvd', tf)

    tf = merge(1, 0, p%write_ice_uvs)
    e = nf90_put_att(ncid, nf90_global, 'write_ice_uvs', tf)

    tf = merge(1, 0, p%write_ice_h_soln)
    e = nf90_put_att(ncid, nf90_global, 'write_ice_h_soln', tf)

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
    
    if (p%write_topo) then
     	e = nf90_def_var(ncid, 'topo', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate)
     	e = nf90_put_att(ncid, vid, 'long_name', 'topography')
     	e = nf90_put_att(ncid, vid, 'units', 'm')
    end if

    if (p%write_topo_dot_ice) then
     	e = nf90_def_var(ncid, 'topo_dot_ice', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'topo_rate_of_change_from_ice')
     	e = nf90_put_att(ncid, vid, 'units', 'm_a')
    end if

    if (p%write_surf) then
     	e = nf90_def_var(ncid, 'surf', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate)
     	e = nf90_put_att(ncid, vid, 'long_name', 'surface_elevation_including_ice')
     	e = nf90_put_att(ncid, vid, 'units', 'm')
    end if

    if (p%write_temp_surf) then
     	e = nf90_def_var(ncid, 'temp_surf', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'surface_temperature')
     	e = nf90_put_att(ncid, vid, 'units', 'C')
    end if

    if (p%write_temp_base) then
     	e = nf90_def_var(ncid, 'temp_base', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_basal_temperature')
     	e = nf90_put_att(ncid, vid, 'units', 'C')
    end if

    if (p%write_temp_ice) then
     	e = nf90_def_var(ncid, 'temp_ice', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_mean_temperature')
     	e = nf90_put_att(ncid, vid, 'units', 'C')
    end if

    if (p%write_precip) then
     	e = nf90_def_var(ncid, 'precip', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'precipitation_rate')
     	e = nf90_put_att(ncid, vid, 'units', 'mwater_a')
    end if

    if (p%write_runoff) then
     	e = nf90_def_var(ncid, 'runoff', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'runoff rate')
     	e = nf90_put_att(ncid, vid, 'units', 'mwater_a')
    end if

    if (p%write_ice_q_surf) then
     	e = nf90_def_var(ncid, 'ice_q_surf', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'surface_ice_flux')
     	e = nf90_put_att(ncid, vid, 'units', 'mice_a')
    end if

    if (p%write_ice_h) then
     	e = nf90_def_var(ncid, 'ice_h', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_thickness')
     	e = nf90_put_att(ncid, vid, 'units', 'm')
    end if

    if (p%write_ice_h_dot) then
     	e = nf90_def_var(ncid, 'ice_h_dot', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_thickness_rate_of_change')
     	e = nf90_put_att(ncid, vid, 'units', 'm_a')
    end if

    if (p%write_ice_uvd) then
     	e = nf90_def_var(ncid, 'ice_ud', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_deformation_velocity_x')
     	e = nf90_put_att(ncid, vid, 'units', 'm_a')

     	e = nf90_def_var(ncid, 'ice_vd', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_deformation_velocity_y')
     	e = nf90_put_att(ncid, vid, 'units', 'm_a')
    end if

    if (p%write_ice_uvs) then
     	e = nf90_def_var(ncid, 'ice_us', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_sliding_velocity_x')
     	e = nf90_put_att(ncid, vid, 'units', 'm_a')

     	e = nf90_def_var(ncid, 'ice_vs', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_sliding_velocity_y')
     	e = nf90_put_att(ncid, vid, 'units', 'm_a')
    end if

    if (p%write_ice_h_soln) then
     	e = nf90_def_var(ncid, 'ice_h_soln', rp_nc, [xid, yid, tid], vid, &
        chunksizes = chunk, shuffle = shuf, deflate_level = deflate )
     	e = nf90_put_att(ncid, vid, 'long_name', 'ice_thickness_solution')
     	e = nf90_put_att(ncid, vid, 'units', 'm')
    end if

    ! populate coordinate variables
    e = nf90_enddef(ncid)
    call req('write_file: exit definition mode: ', e)

    e = nf90_inq_varid(ncid, 'x', vid)
    e = nf90_put_var(ncid, vid, s%x(2:p%nx-1))
    call req('write_file: populate x coordinate: ', e)

    e = nf90_inq_varid(ncid, 'y', vid)
    e = nf90_put_var(ncid, vid, s%y(2:p%ny-1))
    call req('write_file: populate y coordinate: ', e)

    ! close file
    e = nf90_close(ncid)
    call req('write_file: close file: ', e)

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

    if (p%write_topo) then
      e = nf90_inq_varid(ncid, 'topo', vid)
      e = nf90_put_var(ncid, vid, s%topo(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if

    if (p%write_topo_dot_ice) then
      e = nf90_inq_varid(ncid, 'topo_dot_ice', vid)
      e = nf90_put_var(ncid, vid, s%topo_dot_ice(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_surf) then
      e = nf90_inq_varid(ncid, 'surf', vid)
      e = nf90_put_var(ncid, vid, s%surf(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if

    if (p%write_temp_surf) then
      e = nf90_inq_varid(ncid, 'temp_surf', vid)
      e = nf90_put_var(ncid, vid, s%temp_surf(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_temp_ice) then
      e = nf90_inq_varid(ncid, 'temp_ice', vid)
      e = nf90_put_var(ncid, vid, s%temp_ice(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_temp_base) then
      e = nf90_inq_varid(ncid, 'temp_base', vid)
      e = nf90_put_var(ncid, vid, s%temp_base(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_precip) then
      e = nf90_inq_varid(ncid, 'precip', vid)
      e = nf90_put_var(ncid, vid, s%precip(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_runoff) then
      e = nf90_inq_varid(ncid, 'runoff', vid)
      e = nf90_put_var(ncid, vid, s%runoff(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_ice_q_surf) then
      e = nf90_inq_varid(ncid, 'ice_q_surf', vid)
      e = nf90_put_var(ncid, vid, s%ice_q_surf(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_ice_h) then
      e = nf90_inq_varid(ncid, 'ice_h', vid)
      e = nf90_put_var(ncid, vid, s%ice_h(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_ice_h_dot) then
      e = nf90_inq_varid(ncid, 'ice_h_dot', vid)
      e = nf90_put_var(ncid, vid, s%ice_h_dot(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_ice_h_soln) then
      e = nf90_inq_varid(ncid, 'ice_h_soln', vid)
      e = nf90_put_var(ncid, vid, s%ice_h_soln(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_ice_uvd) then
      e = nf90_inq_varid(ncid, 'ice_ud', vid)
      e = nf90_put_var(ncid, vid, s%ice_ud(2:p%nx-1,2:p%ny-1), [1, 1, n])
      
      e = nf90_inq_varid(ncid, 'ice_vd', vid)
      e = nf90_put_var(ncid, vid, s%ice_vd(2:p%nx-1,2:p%ny-1), [1, 1, n])
    end if
    
    if (p%write_ice_uvs) then
      e = nf90_inq_varid(ncid, 'ice_us', vid)
      e = nf90_put_var(ncid, vid, s%ice_us(2:p%nx-1,2:p%ny-1), [1, 1, n])
    
      e = nf90_inq_varid(ncid, 'ice_vs', vid)
      e = nf90_put_var(ncid, vid, s%ice_vs(2:p%nx-1,2:p%ny-1), [1, 1, n])
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

    if (p%write_topo) then
      print "('TOPO (max, mean, min) [m]        : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%topo), &
        meanval_intr(s%topo), &
        minval_intr(s%topo)
    end if

    if (p%write_topo_dot_ice) then
      print "('TOPO_DOT_ICE (max, mean, min) [m]: ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%topo_dot_ice), &
        meanval_intr(s%topo_dot_ice), &
        minval_intr(s%topo_dot_ice)
    end if

    if (p%write_surf) then
      print "('SURF (max, mean, min) [m]        : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%surf), &
        meanval_intr(s%surf), &
        minval_intr(s%surf)
    end if

    if (p%write_temp_surf) then
      print "('TEMP_SURF (max, mean, min) [C]   : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%temp_surf), &
        meanval_intr(s%temp_surf), &
        minval_intr(s%temp_surf)
    end if

    if (p%write_temp_base) then
      print "('TEMP_BASE (max, mean, min) [C]   : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%temp_base), &
        meanval_intr(s%temp_base), &
        minval_intr(s%temp_base)
    end if

    if (p%write_temp_ice) then
      print "('TEMP_ICE (max, mean, min) [C]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%temp_ice), &
        meanval_intr(s%temp_ice), &
        minval_intr(s%temp_ice)
    end if

    if (p%write_precip) then
      print "('PRECIP (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%precip), &
        meanval_intr(s%precip), &
        minval_intr(s%precip)
    end if

    if (p%write_runoff) then
      print "('RUNOFF (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%runoff), &
        meanval_intr(s%runoff), &
        minval_intr(s%runoff)
    end if

    if (p%write_ice_q_surf) then
      print "('ICE_Q_SURF (max, mean, min) [m/a]: ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_q_surf), &
        meanval_intr(s%ice_q_surf), &
        minval_intr(s%ice_q_surf)
    end if
    
    if (p%write_ice_h) then
      print "('ICE_H (max, mean, min) [m]       : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_h), &
        meanval_intr(s%ice_h), &
        minval_intr(s%ice_h)
    end if

    if (p%write_ice_h_dot) then
      print "('ICE_H_DOT (max, mean, min) [m/a] : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_h_dot), &
        meanval_intr(s%ice_h_dot), &
        minval_intr(s%ice_h_dot)
    end if

    if (p%write_ice_uvd) then
      print "('ICE_UD (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_ud), &
        meanval_intr(s%ice_ud)/size(s%ice_ud), &
        minval_intr(s%ice_ud)
      print "('ICE_VD (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_vd), &
        meanval_intr(s%ice_vd), &
        minval_intr(s%ice_vd)
    end if

    if (p%write_ice_uvs) then
      print "('ICE_US (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_us), &
        meanval_intr(s%ice_us), &
        minval_intr(s%ice_us)
      print "('ICE_VS (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_vs), &
        meanval_intr(s%ice_vs), &
        minval_intr(s%ice_vs)
    end if

    if (p%write_ice_h_soln) then
      print "('ICE_H_SOLN (max, mean, min) [m]  : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_h_soln), &
        meanval_intr(s%ice_h_soln), &
        minval_intr(s%ice_h_soln)
    end if

    print *, ''

  end subroutine write_status

end module io
