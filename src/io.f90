! =============================================================================
! Input/output variables and procedures routines for ice-cascade.
!
! Contains:
!   io_type: Public, object for io vars and methods
!   
! ============================================================================

module io_mod

use kinds_mod, only: rp, rp_nc
use param_mod, only: param_type
use state_mod, only: state_type
use netcdf

implicit none
private
public :: io_type


  ! ---------------------------------------------------------------------------
  ! TYPE: input/output vars and procedures
  ! ---------------------------------------------------------------------------
  type io_type
    !integer :: n_step ! counter for current output step
  contains
    procedure, nopass :: read_param ! read parameters from the input file
    procedure, nopass :: read_var ! read initial values 
    procedure, nopass :: write_file ! create output file
    procedure, nopass :: write_step ! append model state to ouput
    !procedure, nopass :: write_status ! print model status to screen
  end type io_type


contains


  ! ---------------------------------------------------------------------------
  ! SUB: Error handling for required netcdf actions 
  !---------------------------------------------------------------------------
  subroutine err_req(str, e)
    
    character(len=*), intent(in) :: str
    integer, intent (in) :: e

    if(e .ne. nf90_noerr) then
      print *, str, trim(nf90_strerror(e))
      stop 'Stopped'
    end if

  end subroutine err_req


  ! ---------------------------------------------------------------------------
  ! SUB: Error handling for optional netcdf actions 
  !---------------------------------------------------------------------------
  subroutine err_opt(str, e)
    
    character(len=*), intent(in) :: str
    integer, intent (in) :: e

    if(e .ne. nf90_noerr) then
      print *, '(optional) ', str, trim(nf90_strerror(e))
    end if

  end subroutine err_opt


  ! ---------------------------------------------------------------------------
  ! SUB: Read input parameters from file
  !   TO_DO: add write flags as attributes
  ! ---------------------------------------------------------------------------
  subroutine read_param(prm)

    type(param_type), intent(out) :: prm
    
    integer :: ncid, e, n, tf

    ! Get input arguments
    select case (command_argument_count())
      case (2)
      	call get_command_argument(1, prm%input_file) 
      	call get_command_argument(2, prm%output_file) 
      case default
      	print*,'ICE-CASCADE expects exactly 2 input arguments'
      	stop -1
    end select

    ! Open file
    e = nf90_open(prm%input_file, nf90_nowrite, ncid)
    call err_req('read_param: open file: ', e)

    ! Read parameters from global attributes
    e = nf90_get_att(ncid, nf90_global, 'nx__1', prm%nx)
    call err_req('read_param: nx__1: ', e)

    e = nf90_get_att(ncid, nf90_global, 'ny__1', prm%ny)
    call err_req('read_param: ny__1: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'lx__m', prm%lx)
    call err_req('read_param: lx__m: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'ly__m', prm%ly)
    call err_req('read_param: ly__m: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'dx__m', prm%dx)
    call err_req('read_param: dx__m: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'dy__m', prm%dy)
    call err_req('read_param: dy__m: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'rhoi__kg_m3', prm%rhoi)
    call err_req('read_param: rhoi__kg_m3: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'grav__m_s2', prm%grav)
    call err_req('read_param: grav__m_s2: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'time_start__a', prm%time_start )
    call err_req('read_param: time_start__a: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'time_finish__a', prm%time_finish)
    call err_req('read_param: time_finish__a: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'time_step__a', prm%time_step)
    call err_req('read_param: time_step__a: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'time_step_write__a', prm%time_step_write)
    call err_req('read_param: time_step_write__a: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'climate_name', prm%climate_name)
    call err_req('read_param: climate_name: ', e)
    
    e = nf90_inquire_attribute(ncid, nf90_global, 'climate_param__var', len = n)
    call err_req('read_param: climate_param__var: ', e)
    allocate(prm%climate_param(n))
    e = nf90_get_att(ncid, nf90_global, 'climate_param__var', prm%climate_param)
    call err_req('read_param: climate_param__var: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'ice_name', prm%ice_name)
    call err_req('read_param: ice_name: ', e)
    
    e = nf90_inquire_attribute(ncid, nf90_global, 'ice_param__var', len = n)
    call err_req('read_param: ice_param__var: ', e)
    allocate(prm%ice_param(n))
    e = nf90_get_att(ncid, nf90_global, 'ice_param__var', prm%ice_param)
    call err_req('read_param: ice_param__var: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'ice_name_bc', prm%ice_bc_name)
    call err_req('read_param: ice_name_bc: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'ice_name_soln', prm%ice_soln_name)
    call err_req('read_param: ice_name_soln: ', e)
    
    e = nf90_inquire_attribute(ncid, nf90_global, 'ice_param_soln__var', len = n)
    call err_req('read_param: ice_param_soln__var: ', e)
    allocate(prm%ice_soln_param(n))
    e = nf90_get_att(ncid, nf90_global, 'ice_param_soln__var', prm%ice_soln_param)
    call err_req('read_param: ice_param_soln__var: ', e)

    e = nf90_get_att(ncid, nf90_global, 'write_topo', tf)
    call err_req('read_param: write_topo: ', e)
    prm%write_topo = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_topo_dot_ice', tf)
    call err_req('read_param: write_topo_dot_ice: ', e)
    prm%write_topo_dot_ice = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_temp_surf', tf)
    call err_req('read_param: write_temp_surf: ', e)
    prm%write_temp_surf = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_temp_ice', tf)
    call err_req('read_param: write_temp_ice: ', e)
    prm%write_temp_ice = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_temp_base', tf)
    call err_req('read_param: write_temp_base: ', e)
    prm%write_temp_base = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_precip', tf)
    call err_req('read_param: write_precip: ', e)
    prm%write_precip = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_runoff', tf)
    call err_req('read_param: write_runoff: ', e)
    prm%write_runoff = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_ice_q_surf', tf)
    call err_req('read_param: write_ice_q_surf: ', e)
    prm%write_ice_q_surf = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_ice_h', tf)
    call err_req('read_param: write_ice_h: ', e)
    prm%write_ice_h = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_ice_h_dot', tf)
    call err_req('read_param: write_ice_h_dot: ', e)
    prm%write_ice_h_dot = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_ice_uvd', tf)
    call err_req('read_param: write_ice_uvd: ', e)
    prm%write_ice_uvd = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_ice_uvs', tf)
    call err_req('read_param: write_ice_uvs: ', e)
    prm%write_ice_uvs = (tf .eq. 1)

    e = nf90_get_att(ncid, nf90_global, 'write_ice_h_soln', tf)
    call err_req('read_param: write_ice_h_soln: ', e)
    prm%write_ice_h_soln = (tf .eq. 1)

    ! Close file
    e = nf90_close(ncid)
    call err_req('read_param: close file: ', e)

  end subroutine read_param


  !----------------------------------------------------------------------------
  ! SUB: Read initial variables from netcdf file
  !   unspecified variables retain the default value (0)
  ! ---------------------------------------------------------------------------
  subroutine read_var(prm, sta)

    type(param_type), intent(in) :: prm
    type(state_type), intent(inout) :: sta

    integer :: e, ncid, varid
    
    ! Open file
    e = nf90_open(prm%input_file, nf90_nowrite, ncid)
    call err_req('read vars: open file: ', e)

    ! Read variables
    e = nf90_inq_varid(ncid, 'x', varid)
    call err_req('read_vars: x: ', e)
    e = nf90_get_var(ncid, varid, sta%x(2:prm%nx+1))
    call err_req('read_vars: x: ', e)
    sta%x(1) = sta%x(2)-prm%dx
    sta%x(prm%nx+2) = sta%x(prm%nx+1)+prm%dx

    e = nf90_inq_varid(ncid, 'y', varid)
    call err_req('read_vars: y: ', e)
    e = nf90_get_var(ncid, varid, sta%y(2:prm%ny+1))
    call err_req('read_vars: y: ', e)
    sta%y(1) = sta%y(2)-prm%dy
    sta%y(prm%ny+2) = sta%y(prm%ny+1)+prm%dy

    e = nf90_inq_varid(ncid, 'topo', varid)
    call err_opt('read_vars: topo: ', e)
    e = nf90_get_var(ncid, varid, sta%topo(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: topo: ', e)

    e = nf90_inq_varid(ncid, 'topo_dot_ice', varid)
    call err_opt('read_vars: topo_dot_ice: ', e)
    e = nf90_get_var(ncid, varid, sta%topo_dot_ice(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: topo_dot_ice: ', e)

    e = nf90_inq_varid(ncid, 'temp_surf', varid)
    call err_opt('read_vars: temp_surf: ', e)
    e = nf90_get_var(ncid, varid, sta%temp_surf(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: temp_surf: ', e)

    e = nf90_inq_varid(ncid, 'temp_ice', varid)
    call err_opt('read_vars: temp_ice: ', e)
    e = nf90_get_var(ncid, varid, sta%temp_ice(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: temp_ice: ', e)
    
    e = nf90_inq_varid(ncid, 'temp_base', varid)
    call err_opt('read_vars: temp_base: ', e)
    e = nf90_get_var(ncid, varid, sta%temp_base(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: temp_base: ', e)

    e = nf90_inq_varid(ncid, 'precip', varid)
    call err_opt('read_vars: precip: ', e)
    e = nf90_get_var(ncid, varid, sta%precip(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: precip: ', e)

    e = nf90_inq_varid(ncid, 'runoff', varid)
    call err_opt('read_vars: runoff: ', e)
    e = nf90_get_var(ncid, varid, sta%runoff(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: runoff: ', e)

    e = nf90_inq_varid(ncid, 'ice_q_surf', varid)
    call err_opt('read_vars: ice_q_surf: ', e)
    e = nf90_get_var(ncid, varid, sta%ice_q_surf(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: ice_q_surf: ', e)

    e = nf90_inq_varid(ncid, 'ice_h', varid)
    call err_opt('read_vars: ice_h: ', e)
    e = nf90_get_var(ncid, varid, sta%ice_h(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: ice_h: ', e)

    e = nf90_inq_varid(ncid, 'ice_h_dot', varid)
    call err_opt('read_vars: ice_h_dot: ', e)
    e = nf90_get_var(ncid, varid, sta%ice_h_dot(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: ice_h_dot: ', e)

    e = nf90_inq_varid(ncid, 'ice_h_soln', varid)
    call err_opt('read_vars: ice_h_soln: ', e)
    e = nf90_get_var(ncid, varid, sta%ice_h_soln(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: ice_h_soln: ', e)

    e = nf90_inq_varid(ncid, 'ice_ud', varid)
    call err_opt('read_vars: ice_ud: ', e)
    e = nf90_get_var(ncid, varid, sta%ice_ud(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: ice_ud: ', e)

    e = nf90_inq_varid(ncid, 'ice_vd', varid)
    call err_opt('read_vars: ice_vd: ', e)
    e = nf90_get_var(ncid, varid, sta%ice_vd(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: ice_vd: ', e)

    e = nf90_inq_varid(ncid, 'ice_us', varid)
    call err_opt('read_vars: ice_us: ', e)
    e = nf90_get_var(ncid, varid, sta%ice_us(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: ice_us: ', e)

    e = nf90_inq_varid(ncid, 'ice_vs', varid)
    call err_opt('read_vars: ice_vs: ', e)
    e = nf90_get_var(ncid, varid, sta%ice_vs(2:prm%nx+1, 2:prm%ny+1))
    call err_opt('read_vars: ice_vs: ', e)

    ! Close file
    e = nf90_close(ncid)
    call err_req('read_param: close file: ', e)

  end subroutine read_var


  ! ---------------------------------------------------------------------------
  ! SUB: Create output netcdf file
  ! ---------------------------------------------------------------------------
  subroutine write_file(p)

    type(param_type), intent(in) :: p

    logical :: shuf
    integer :: chunk(3), deflate, e, ncid, tf, tid, vid, xid, yid

    ! define compression and chunking parameters 
    chunk = [p%nx, p%ny, 1] ! x, y, t
    deflate = 1 ! compression, 0 = none, 9 = max, best value is 1
    shuf = .true.

    ! create new file
    e = nf90_create(p%output_file, nf90_netcdf4, ncid)
    call err_req('write_file: create file: ', e)

!    ! write parameters as global attributes
    e = nf90_put_att(ncid, nf90_global, 'nx__1', p%nx)

    e = nf90_put_att(ncid, nf90_global, 'ny__1', p%ny)
    
    e = nf90_put_att(ncid, nf90_global, 'lx__m', p%lx)
    
    e = nf90_put_att(ncid, nf90_global, 'ly__m', p%ly)
    
    e = nf90_put_att(ncid, nf90_global, 'dx__m', p%dx)
    
    e = nf90_put_att(ncid, nf90_global, 'dy__m', p%dy)
    
    e = nf90_put_att(ncid, nf90_global, 'rhoi__kg_m3', p%rhoi)
    
    e = nf90_put_att(ncid, nf90_global, 'grav__m_s2', p%grav)
    
    e = nf90_put_att(ncid, nf90_global, 'time_start__a', p%time_start )
    
    e = nf90_put_att(ncid, nf90_global, 'time_finish__a', p%time_finish)
    
    e = nf90_put_att(ncid, nf90_global, 'time_step__a', p%time_step)
    
    e = nf90_put_att(ncid, nf90_global, 'time_step_write__a', p%time_step_write)
    
    e = nf90_put_att(ncid, nf90_global, 'climate_name', p%climate_name)
    
    e = nf90_put_att(ncid, nf90_global, 'climate_param__var', p%climate_param)
    
    e = nf90_put_att(ncid, nf90_global, 'ice_name', p%ice_name)
    
    e = nf90_put_att(ncid, nf90_global, 'ice_param__var', p%ice_param)
    
    e = nf90_put_att(ncid, nf90_global, 'ice_name_bc', p%ice_bc_name)
    
    e = nf90_put_att(ncid, nf90_global, 'ice_name_soln', p%ice_soln_name)
    
    e = nf90_put_att(ncid, nf90_global, 'ice_param_soln__var', p%ice_soln_param)

    tf = merge(1, 0, p%write_topo)
    e = nf90_put_att(ncid, nf90_global, 'write_topo', tf)

    tf = merge(1, 0, p%write_topo_dot_ice)
    e = nf90_put_att(ncid, nf90_global, 'write_topo_dot_ice', tf)

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
    e = nf90_def_dim(ncid, 'x', p%nx, xid) 
    e = nf90_def_dim(ncid, 'y', p%ny, yid) 
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

    ! close file
    e = nf90_close(ncid)
    call err_req('write_file: close file: ', e)

  end subroutine write_file

  ! ---------------------------------------------------------------------------
  ! SUB: Append model state to output netcdf
  ! ---------------------------------------------------------------------------
  subroutine write_step(p, s)
    
    type(param_type), intent(inout) :: p
    type(state_type), intent(inout) :: s

    integer :: e, i0, i1, j0, j1, n, ncid, tid, vid

    ! define limits of interior points, for convenience
    i0 = 2; i1 = p%nx+1
    j0 = 2; j1 = p%ny+1

    ! open file
    e = nf90_open(p%output_file, nf90_write, ncid)

    ! get current step
    e = nf90_inq_dimid(ncid, 't', tid)
    e = nf90_inquire_dimension(ncid, tid, len = n)
    n = n+1

    ! write data
    e = nf90_inq_varid(ncid, 't', vid)
    e = nf90_put_var(ncid, vid, s%time_now, [n] )

    if (p%write_topo) then
      e = nf90_inq_varid(ncid, 'topo', vid)
      e = nf90_put_var(ncid, vid, s%topo(i0:i1, j0:j1), [1, 1, n])
    end if

    if (p%write_topo_dot_ice) then
      e = nf90_inq_varid(ncid, 'topo_dot_ice', vid)
      e = nf90_put_var(ncid, vid, s%topo_dot_ice(i0:i1, j0:j1), [1, 1, n])
    end if
    
    if (p%write_temp_surf) then
      e = nf90_inq_varid(ncid, 'temp_surf', vid)
      e = nf90_put_var(ncid, vid, s%temp_surf(i0:i1, j0:j1), [1, 1, n])
    end if
    
    if (p%write_temp_ice) then
      e = nf90_inq_varid(ncid, 'temp_ice', vid)
      e = nf90_put_var(ncid, vid, s%temp_ice(i0:i1, j0:j1), [1, 1, n])
    end if
    
    if (p%write_temp_base) then
      e = nf90_inq_varid(ncid, 'temp_base', vid)
      e = nf90_put_var(ncid, vid, s%temp_base(i0:i1, j0:j1), [1, 1, n])
    end if
    
    if (p%write_precip) then
      e = nf90_inq_varid(ncid, 'precip', vid)
      e = nf90_put_var(ncid, vid, s%precip(i0:i1, j0:j1), [1, 1, n])
    end if
    
    if (p%write_runoff) then
      e = nf90_inq_varid(ncid, 'runoff', vid)
      e = nf90_put_var(ncid, vid, s%runoff(i0:i1, j0:j1), [1, 1, n])
    end if
    
    if (p%write_ice_q_surf) then
      e = nf90_inq_varid(ncid, 'ice_q_surf', vid)
      e = nf90_put_var(ncid, vid, s%ice_q_surf(i0:i1, j0:j1), [1, 1, n])
    end if
    
    if (p%write_ice_h) then
      e = nf90_inq_varid(ncid, 'ice_h', vid)
      e = nf90_put_var(ncid, vid, s%ice_h(i0:i1, j0:j1), [1, 1, n])
    end if
    
    if (p%write_ice_h_dot) then
      e = nf90_inq_varid(ncid, 'ice_h_dot', vid)
      e = nf90_put_var(ncid, vid, s%ice_h_dot(i0:i1, j0:j1), [1, 1, n])
    end if
    
    if (p%write_ice_h_soln) then
      e = nf90_inq_varid(ncid, 'ice_h_soln', vid)
      e = nf90_put_var(ncid, vid, s%ice_h_soln(i0:i1, j0:j1), [1, 1, n])
    end if
    
    if (p%write_ice_uvd) then
      e = nf90_inq_varid(ncid, 'ice_ud', vid)
      e = nf90_put_var(ncid, vid, s%ice_ud(i0:i1, j0:j1), [1, 1, n])
      
      e = nf90_inq_varid(ncid, 'ice_vd', vid)
      e = nf90_put_var(ncid, vid, s%ice_vd(i0:i1, j0:j1), [1, 1, n])
    end if
    
    if (p%write_ice_uvs) then
      e = nf90_inq_varid(ncid, 'ice_us', vid)
      e = nf90_put_var(ncid, vid, s%ice_us(i0:i1, j0:j1), [1, 1, n])
    
      e = nf90_inq_varid(ncid, 'ice_vs', vid)
      e = nf90_put_var(ncid, vid, s%ice_vs(i0:i1, j0:j1), [1, 1, n])
    end if

    ! close file
    e = nf90_close(ncid)

  end subroutine write_step


!  ! --------------------------------------------------------------------------
!  ! SUB: print model status update to stdout
!  ! --------------------------------------------------------------------------
!  subroutine write_status(s)
!    
!    type(state_type), intent(in) :: s
!
!    print "('MODEL TIME [a]           : ', EN11.3)", s%time_now 
!    print "('TOPO (max, mean, min) [m]: ', EN11.3, ', ', EN11.3, ', ', EN11.3)", &
!          maxval(s%topo), sum(s%topo)/size(s%topo), minval(s%topo)
!    print *, ''
!
!  end subroutine
!
!

end module io_mod
