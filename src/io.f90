! =============================================================================
! Input/output procedures for ice-cascade.
!
! Description: The model reads from and writes to netCDF files with a specific
!   format. All the expected parameters MUST be defined as global parameters in
!   the input file. Missing parameters will cause the program to exit with an
!   error. All the state variables MAY be defined in the input file, in which
!   case they are read in as the initial values for these variables. Any missing
!   variables will be set to the default value (0). However, note that the
!   various model components are run at the initial time, and may overwrite the
!   supplied input data (e.g. if you provide an intitial temperature in the
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
  subroutine req(err)
  ! 
    integer, intent (in) :: err
  !
  ! ABOUT: Error handling for required netcdf actions 
  !---------------------------------------------------------------------------

    if(err .ne. nf90_noerr) then
      print *, '(REQUIRED) ', trim(nf90_strerror(err))
      stop 
    end if

  end subroutine req 


  ! ---------------------------------------------------------------------------
  subroutine opt(err)
  ! 
    integer, intent (in) :: err
  !
  ! ABOUT: Error handling for optional netcdf actions 
  !---------------------------------------------------------------------------

    if(err .ne. nf90_noerr) then
      print *, '(OPTIONAL) ', trim(nf90_strerror(err))
    end if

  end subroutine opt


  ! ---------------------------------------------------------------------------
  subroutine read_param(p)
  !
    type(param_type), intent(out) :: p
  !
  ! ABOUT: Read input parameters from file
  ! ---------------------------------------------------------------------------
    
    integer :: ncid, n

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
    call req(nf90_open(p%input_file, nf90_nowrite, ncid))

    ! Allocate vector attributes
    call req(nf90_inquire_attribute(ncid, nf90_global, 'time_write__a', len = n))
    allocate(p%time_write(n))
    call req(nf90_inquire_attribute(ncid, nf90_global, 'climate_param__var', len = n))
    allocate(p%climate_param(n))
    call req(nf90_inquire_attribute(ncid, nf90_global, 'ice_param__var', len = n))
    allocate(p%ice_param(n))
    call req(nf90_inquire_attribute(ncid, nf90_global, 'ice_soln_param__var', len = n))
    allocate(p%ice_soln_param(n))

    ! Read parameters from global attributes
    call req(nf90_get_att(ncid, nf90_global, 'nx__1', p%nx))
    call req(nf90_get_att(ncid, nf90_global, 'ny__1', p%ny))
    call req(nf90_get_att(ncid, nf90_global, 'lx__m', p%lx))
    call req(nf90_get_att(ncid, nf90_global, 'ly__m', p%ly))
    call req(nf90_get_att(ncid, nf90_global, 'dx__m', p%dx))
    call req(nf90_get_att(ncid, nf90_global, 'dy__m', p%dy))
    call req(nf90_get_att(ncid, nf90_global, 'rhoi__kg_m3', p%rhoi))
    call req(nf90_get_att(ncid, nf90_global, 'grav__m_s2', p%grav))
    call req(nf90_get_att(ncid, nf90_global, 'time_start__a', p%time_start))
    call req(nf90_get_att(ncid, nf90_global, 'time_finish__a', p%time_finish))
    call req(nf90_get_att(ncid, nf90_global, 'time_step__a', p%time_step))
    call req(nf90_get_att(ncid, nf90_global, 'time_write__a', p%time_write))
    call req(nf90_get_att(ncid, nf90_global, 'climate_name', p%climate_name))
    call req(nf90_get_att(ncid, nf90_global, 'climate_param__var', p%climate_param))
    call req(nf90_get_att(ncid, nf90_global, 'ice_name', p%ice_name))
    call req(nf90_get_att(ncid, nf90_global, 'ice_param__var', p%ice_param))
    call req(nf90_get_att(ncid, nf90_global, 'ice_bc_name__nesw', p%ice_bc_name))
    call req(nf90_get_att(ncid, nf90_global, 'ice_soln_name', p%ice_soln_name))
    call req(nf90_get_att(ncid, nf90_global, 'ice_soln_param__var', p%ice_soln_param))
    call req(nf90_get_att(ncid, nf90_global, 'write_topo', p%write_topo))
    call req(nf90_get_att(ncid, nf90_global, 'write_topo_dot_ice', p%write_topo_dot_ice ));
    call req(nf90_get_att(ncid, nf90_global, 'write_surf', p%write_surf));
    call req(nf90_get_att(ncid, nf90_global, 'write_temp_surf', p%write_temp_surf));
    call req(nf90_get_att(ncid, nf90_global, 'write_temp_ice', p%write_temp_ice));
    call req(nf90_get_att(ncid, nf90_global, 'write_temp_base', p%write_temp_base));
    call req(nf90_get_att(ncid, nf90_global, 'write_precip', p%write_precip));
    call req(nf90_get_att(ncid, nf90_global, 'write_runoff', p%write_runoff));
    call req(nf90_get_att(ncid, nf90_global, 'write_ice_q_surf', p%write_ice_q_surf));
    call req(nf90_get_att(ncid, nf90_global, 'write_ice_h', p%write_ice_h));
    call req(nf90_get_att(ncid, nf90_global, 'write_ice_h_dot', p%write_ice_h_dot));
    call req(nf90_get_att(ncid, nf90_global, 'write_ice_uv_defm', p%write_ice_uv_defm));
    call req(nf90_get_att(ncid, nf90_global, 'write_ice_uv_slid', p%write_ice_uv_slid));
    call req(nf90_get_att(ncid, nf90_global, 'write_ice_h_soln', p%write_ice_h_soln));
    call req(nf90_get_att(ncid, nf90_global, 'write_ice_a_defm', p%write_ice_a_defm));
    call req(nf90_get_att(ncid, nf90_global, 'write_ice_a_slid', p%write_ice_a_slid));

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

    integer :: ncid, varid

    ! Open file
    call req(nf90_open(p%input_file, nf90_nowrite, ncid))

    ! Read variables
    call req(nf90_inq_varid(ncid, 'x', varid))
    call req(nf90_get_var(ncid, varid, s%x(2:p%nx-1))) 
    s%x(1) = s%x(2)-p%dx
    s%x(p%nx) = s%x(p%nx-1)+p%dx

    call req(nf90_inq_varid(ncid, 'y', varid))
    call req(nf90_get_var(ncid, varid, s%y(2:p%ny-1))) 
    s%y(1) = s%y(2)-p%dy
    s%y(p%ny) = s%y(p%ny-1)+p%dy
    
    call req(nf90_inq_varid(ncid, 'topo', varid))
    call req(nf90_get_var(ncid, varid, s%topo(2:p%nx-1,2:p%ny-1)))
    
    call req(nf90_inq_varid(ncid, 'topo_dot_ice', varid))
    call req(nf90_get_var(ncid, varid, s%topo_dot_ice(2:p%nx-1,2:p%ny-1)))
    
    call opt(nf90_inq_varid(ncid, 'surf', varid)) 
    call opt(nf90_get_var(ncid, varid, s%surf(2:p%nx-1,2:p%ny-1)))

    call opt(nf90_inq_varid(ncid, 'temp_surf', varid))
    call opt(nf90_get_var(ncid, varid, s%temp_surf(2:p%nx-1,2:p%ny-1)))

    call opt(nf90_inq_varid(ncid, 'temp_ice', varid))
    call opt(nf90_get_var(ncid, varid, s%temp_ice(2:p%nx-1,2:p%ny-1)))
    
    call opt(nf90_inq_varid(ncid, 'temp_base', varid))
    call opt(nf90_get_var(ncid, varid, s%temp_base(2:p%nx-1,2:p%ny-1)))
    
    call opt(nf90_inq_varid(ncid, 'precip', varid))
    call opt(nf90_get_var(ncid, varid, s%precip(2:p%nx-1,2:p%ny-1)))
    
    call opt(nf90_inq_varid(ncid, 'runoff', varid))
    call opt(nf90_get_var(ncid, varid, s%runoff(2:p%nx-1,2:p%ny-1)))
    
    call opt(nf90_inq_varid(ncid, 'ice_q_surf', varid))
    call opt(nf90_get_var(ncid, varid, s%ice_q_surf(2:p%nx-1,2:p%ny-1)))
    
    call opt(nf90_inq_varid(ncid, 'ice_h', varid))
    call opt(nf90_get_var(ncid, varid, s%ice_h(2:p%nx-1,2:p%ny-1)))
    
    call opt(nf90_inq_varid(ncid, 'ice_h_dot', varid))
    call opt(nf90_get_var(ncid, varid, s%ice_h_dot(2:p%nx-1,2:p%ny-1)))
    
    call opt(nf90_inq_varid(ncid, 'ice_h_soln', varid))
    call opt(nf90_get_var(ncid, varid, s%ice_h_soln(2:p%nx-1,2:p%ny-1)))
    
    call opt(nf90_inq_varid(ncid, 'ice_u_defm', varid))
    call opt(nf90_get_var(ncid, varid, s%ice_u_defm(2:p%nx-1,2:p%ny-1)))
    
    call opt(nf90_inq_varid(ncid, 'ice_v_defm', varid))
    call opt(nf90_get_var(ncid, varid, s%ice_v_defm(2:p%nx-1,2:p%ny-1)))
    
    call opt(nf90_inq_varid(ncid, 'ice_u_slid', varid))
    call opt(nf90_get_var(ncid, varid, s%ice_u_slid(2:p%nx-1,2:p%ny-1)))
    
    call opt(nf90_inq_varid(ncid, 'ice_v_slid', varid))
    call opt(nf90_get_var(ncid, varid, s%ice_v_slid(2:p%nx-1,2:p%ny-1)))

    call opt(nf90_inq_varid(ncid, 'ice_a_defm', varid))
    call opt(nf90_get_var(ncid, varid, s%ice_a_defm(2:p%nx-1,2:p%ny-1)))

    call opt(nf90_inq_varid(ncid, 'ice_a_slid', varid))
    call opt(nf90_get_var(ncid, varid, s%ice_a_slid(2:p%nx-1,2:p%ny-1)))

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
    integer :: chunk(3), deflate, ncid, tf, tid, vid, xid, yid

    ! define compression and chunking parameters 
    chunk = [p%nx-2, p%ny-2, 1] ! x, y, t
    deflate = 1 ! compression, 0 = none, 9 = max, best value is 1
    shuf = .true.

    ! create new file
    call req(nf90_create(p%output_file, nf90_netcdf4, ncid))

    ! write parameters as global attributes
    call req(nf90_put_att(ncid, nf90_global, 'nx__1', p%nx-2))
    call req(nf90_put_att(ncid, nf90_global, 'ny__1', p%ny-2))
    call req(nf90_put_att(ncid, nf90_global, 'lx__m', p%lx))
    call req(nf90_put_att(ncid, nf90_global, 'ly__m', p%ly))
    call req(nf90_put_att(ncid, nf90_global, 'dx__m', p%dx))
    call req(nf90_put_att(ncid, nf90_global, 'dy__m', p%dy))
    call req(nf90_put_att(ncid, nf90_global, 'rhoi__kg_m3', p%rhoi))
    call req(nf90_put_att(ncid, nf90_global, 'grav__m_s2', p%grav))
    call req(nf90_put_att(ncid, nf90_global, 'time_start__a', p%time_start ))
    call req(nf90_put_att(ncid, nf90_global, 'time_finish__a', p%time_finish))
    call req(nf90_put_att(ncid, nf90_global, 'time_step__a', p%time_step))
    call req(nf90_put_att(ncid, nf90_global, 'time_write__a', p%time_write))
    call req(nf90_put_att(ncid, nf90_global, 'climate_name', p%climate_name))
    call req(nf90_put_att(ncid, nf90_global, 'climate_param__var', p%climate_param))
    call req(nf90_put_att(ncid, nf90_global, 'ice_name', p%ice_name))
    call req(nf90_put_att(ncid, nf90_global, 'ice_param__var', p%ice_param))
    call req(nf90_put_att(ncid, nf90_global, 'ice_bc_name__nesw', p%ice_bc_name))
    call req(nf90_put_att(ncid, nf90_global, 'ice_soln_name', p%ice_soln_name))
    call req(nf90_put_att(ncid, nf90_global, 'ice_soln_param__var', p%ice_soln_param))
    call req(nf90_put_att(ncid, nf90_global, 'write_topo', p%write_topo))
    call req(nf90_put_att(ncid, nf90_global, 'write_topo_dot_ice', p%write_topo_dot_ice))
    call req(nf90_put_att(ncid, nf90_global, 'write_surf', p%write_surf))
    call req(nf90_put_att(ncid, nf90_global, 'write_temp_surf', p%write_temp_surf))
    call req(nf90_put_att(ncid, nf90_global, 'write_temp_ice', p%write_temp_ice))
    call req(nf90_put_att(ncid, nf90_global, 'write_temp_base', p%write_temp_base))
    call req(nf90_put_att(ncid, nf90_global, 'write_precip', p%write_precip))
    call req(nf90_put_att(ncid, nf90_global, 'write_runoff', p%write_runoff))
    call req(nf90_put_att(ncid, nf90_global, 'write_ice_q_surf', p%write_ice_q_surf))
    call req(nf90_put_att(ncid, nf90_global, 'write_ice_h', p%write_ice_h))
    call req(nf90_put_att(ncid, nf90_global, 'write_ice_h_dot', p%write_ice_h_dot))
    call req(nf90_put_att(ncid, nf90_global, 'write_ice_uv_defm', p%write_ice_uv_defm))
    call req(nf90_put_att(ncid, nf90_global, 'write_ice_uv_slid', p%write_ice_uv_slid))
    call req(nf90_put_att(ncid, nf90_global, 'write_ice_h_soln', p%write_ice_h_soln))
    call req(nf90_put_att(ncid, nf90_global, 'write_ice_a_defm', p%write_ice_a_defm))
    call req(nf90_put_att(ncid, nf90_global, 'write_ice_a_slid', p%write_ice_a_slid))

    ! define dimensions
    call req(nf90_def_dim(ncid, 'x', p%nx-2, xid)) 
    call req(nf90_def_dim(ncid, 'y', p%ny-2, yid)) 
    call req(nf90_def_dim(ncid, 't', nf90_unlimited, tid))

    ! define variables
    call req(nf90_def_var(ncid, 'x', rp_nc, xid, vid))
    call req(nf90_put_att(ncid, vid, 'long_name', 'x_coord'))
    call req(nf90_put_att(ncid, vid, 'units', 'm'))
    
    call req(nf90_def_var(ncid, 'y', rp_nc, yid, vid))
    call req(nf90_put_att(ncid, vid, 'long_name', 'y_coord'))
    call req(nf90_put_att(ncid, vid, 'units', 'm'))

    call req(nf90_def_var(ncid, 't', rp_nc, tid, vid))
    call req(nf90_put_att(ncid, vid, 'long_name', 'model_time'))
    call req(nf90_put_att(ncid, vid, 'units', 'a'))
    
    if (p%write_topo .eq. 1) then
     	call req(nf90_def_var(ncid, 'topo', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'topography'))
     	call req(nf90_put_att(ncid, vid, 'units', 'm'))
    end if

    if (p%write_topo_dot_ice .eq. 1) then
     	call req(nf90_def_var(ncid, 'topo_dot_ice', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'topo_rate_of_change_from_ice'))
     	call req(nf90_put_att(ncid, vid, 'units', 'm_a'))
    end if

    if (p%write_surf .eq. 1) then
     	call req(nf90_def_var(ncid, 'surf', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'surface_elevation_including_ice'))
     	call req(nf90_put_att(ncid, vid, 'units', 'm'))
    end if

    if (p%write_temp_surf .eq. 1) then
     	call req(nf90_def_var(ncid, 'temp_surf', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'surface_temperature'))
     	call req(nf90_put_att(ncid, vid, 'units', 'C'))
    end if

    if (p%write_temp_base .eq. 1) then
     	call req(nf90_def_var(ncid, 'temp_base', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'ice_basal_temperature'))
     	call req(nf90_put_att(ncid, vid, 'units', 'C'))
    end if

    if (p%write_temp_ice .eq. 1) then
     	call req(nf90_def_var(ncid, 'temp_ice', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'ice_mean_temperature'))
     	call req(nf90_put_att(ncid, vid, 'units', 'C'))
    end if

    if (p%write_precip .eq. 1) then
     	call req(nf90_def_var(ncid, 'precip', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'precipitation_rate'))
     	call req(nf90_put_att(ncid, vid, 'units', 'mwater_a'))
    end if

    if (p%write_runoff .eq. 1) then
     	call req(nf90_def_var(ncid, 'runoff', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'runoff rate'))
     	call req(nf90_put_att(ncid, vid, 'units', 'mwater_a'))
    end if

    if (p%write_ice_q_surf .eq. 1) then
     	call req(nf90_def_var(ncid, 'ice_q_surf', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'surface_ice_flux'))
     	call req(nf90_put_att(ncid, vid, 'units', 'mice_a'))
    end if

    if (p%write_ice_h .eq. 1) then
     	call req(nf90_def_var(ncid, 'ice_h', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'ice_thickness'))
     	call req(nf90_put_att(ncid, vid, 'units', 'm'))
    end if

    if (p%write_ice_h_dot .eq. 1) then
     	call req(nf90_def_var(ncid, 'ice_h_dot', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'ice_thickness_rate_of_change'))
     	call req(nf90_put_att(ncid, vid, 'units', 'm_a'))
    end if

    if (p%write_ice_uv_defm .eq. 1) then
     	call req(nf90_def_var(ncid, 'ice_ud', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'ice_deformation_velocity_x'))
     	call req(nf90_put_att(ncid, vid, 'units', 'm_a'))

     	call req(nf90_def_var(ncid, 'ice_vd', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'ice_deformation_velocity_y'))
     	call req(nf90_put_att(ncid, vid, 'units', 'm_a'))
    end if

    if (p%write_ice_uv_slid .eq. 1) then
     	call req(nf90_def_var(ncid, 'ice_us', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'ice_sliding_velocity_x'))
     	call req(nf90_put_att(ncid, vid, 'units', 'm_a'))

     	call req(nf90_def_var(ncid, 'ice_vs', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'ice_sliding_velocity_y'))
     	call req(nf90_put_att(ncid, vid, 'units', 'm_a'))
    end if

    if (p%write_ice_h_soln .eq. 1) then
     	call req(nf90_def_var(ncid, 'ice_h_soln', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'ice_thickness_solution'))
     	call req(nf90_put_att(ncid, vid, 'units', 'm'))
    end if

    if (p%write_ice_a_defm .eq. 1) then
     	call req(nf90_def_var(ncid, 'ice_a_defm', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'ice_deformation_coefficient'))
     	call req(nf90_put_att(ncid, vid, 'units', '1_Pa3_a'))
    end if

    if (p%write_ice_a_slid .eq. 1) then
     	call req(nf90_def_var(ncid, 'ice_a_slid', rp_nc, [xid, yid, tid], vid, .false., chunk, deflate, shuf))
     	call req(nf90_put_att(ncid, vid, 'long_name', 'ice_sliding_coefficient'))
     	call req(nf90_put_att(ncid, vid, 'units', 'm_Pa_a'))
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

    integer :: n, ncid, tid, vid

    ! open file
    call req(nf90_open(p%output_file, nf90_write, ncid))

    ! get current step
    call req(nf90_inq_dimid(ncid, 't', tid))
    call req(nf90_inquire_dimension(ncid, tid, len = n))
    n = n+1

    ! write data
    call req(nf90_inq_varid(ncid, 't', vid))
    call req(nf90_put_var(ncid, vid, s%now, [n]))

    if (p%write_topo .eq. 1) then
      call req(nf90_inq_varid(ncid, 'topo', vid))
      call req(nf90_put_var(ncid, vid, s%topo(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if

    if (p%write_topo_dot_ice .eq. 1) then
      call req(nf90_inq_varid(ncid, 'topo_dot_ice', vid))
      call req(nf90_put_var(ncid, vid, s%topo_dot_ice(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if
    
    if (p%write_surf .eq. 1) then
      call req(nf90_inq_varid(ncid, 'surf', vid))
      call req(nf90_put_var(ncid, vid, s%surf(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if

    if (p%write_temp_surf .eq. 1) then
      call req(nf90_inq_varid(ncid, 'temp_surf', vid))
      call req(nf90_put_var(ncid, vid, s%temp_surf(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if
    
    if (p%write_temp_ice .eq. 1) then
      call req(nf90_inq_varid(ncid, 'temp_ice', vid))
      call req(nf90_put_var(ncid, vid, s%temp_ice(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if
    
    if (p%write_temp_base .eq. 1) then
      call req(nf90_inq_varid(ncid, 'temp_base', vid))
      call req(nf90_put_var(ncid, vid, s%temp_base(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if
    
    if (p%write_precip .eq. 1) then
      call req(nf90_inq_varid(ncid, 'precip', vid))
      call req(nf90_put_var(ncid, vid, s%precip(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if
    
    if (p%write_runoff .eq. 1) then
      call req(nf90_inq_varid(ncid, 'runoff', vid))
      call req(nf90_put_var(ncid, vid, s%runoff(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if
    
    if (p%write_ice_q_surf .eq. 1) then
      call req(nf90_inq_varid(ncid, 'ice_q_surf', vid))
      call req(nf90_put_var(ncid, vid, s%ice_q_surf(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if
    
    if (p%write_ice_h .eq. 1) then
      call req(nf90_inq_varid(ncid, 'ice_h', vid))
      call req(nf90_put_var(ncid, vid, s%ice_h(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if
    
    if (p%write_ice_h_dot .eq. 1) then
      call req(nf90_inq_varid(ncid, 'ice_h_dot', vid))
      call req(nf90_put_var(ncid, vid, s%ice_h_dot(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if
    
    if (p%write_ice_h_soln .eq. 1) then
      call req(nf90_inq_varid(ncid, 'ice_h_soln', vid))
      call req(nf90_put_var(ncid, vid, s%ice_h_soln(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if
    
    if (p%write_ice_uv_defm .eq. 1) then
      call req(nf90_inq_varid(ncid, 'ice_u_defm', vid))
      call req(nf90_put_var(ncid, vid, s%ice_u_defm(2:p%nx-1,2:p%ny-1), [1, 1, n]))
      
      call req(nf90_inq_varid(ncid, 'ice_v_defm', vid))
      call req(nf90_put_var(ncid, vid, s%ice_v_defm(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if
    
    if (p%write_ice_uv_slid .eq. 1) then
      call req(nf90_inq_varid(ncid, 'ice_u_slid', vid))
      call req(nf90_put_var(ncid, vid, s%ice_u_slid(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    
      call req(nf90_inq_varid(ncid, 'ice_v_slid', vid))
      call req(nf90_put_var(ncid, vid, s%ice_v_slid(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if

    if (p%write_ice_a_defm .eq. 1) then
      call req(nf90_inq_varid(ncid, 'ice_a_defm', vid))
      call req(nf90_put_var(ncid, vid, s%ice_a_defm(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if

    if (p%write_ice_a_slid .eq. 1) then
      call req(nf90_inq_varid(ncid, 'ice_a_slid', vid))
      call req(nf90_put_var(ncid, vid, s%ice_a_slid(2:p%nx-1,2:p%ny-1), [1, 1, n]))
    end if

    ! close file
    call req(nf90_close(ncid))

  end subroutine write_step


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
  
  
  ! --------------------------------------------------------------------------
  subroutine write_status(p, s)
  ! 
    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s
  !
  ! ABOUT: print model status update to stdout
  ! --------------------------------------------------------------------------

    print "('TIME [a]                               : ', EN12.3)", s%now 

    if (p%write_topo .eq. 1)  &
      print "('TOPO (max, mean, min) [m]            : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%topo), meanval_intr(s%topo), minval_intr(s%topo)

    if (p%write_topo_dot_ice .eq. 1) &
      print "('TOPO_DOT_ICE (max, mean, min) [m]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%topo_dot_ice), meanval_intr(s%topo_dot_ice), minval_intr(s%topo_dot_ice)

    if (p%write_surf .eq. 1) &
      print "('SURF (max, mean, min) [m]            : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%surf), meanval_intr(s%surf), minval_intr(s%surf)

    if (p%write_temp_surf .eq. 1) &
      print "('TEMP_SURF (max, mean, min) [C]       : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%temp_surf), meanval_intr(s%temp_surf), minval_intr(s%temp_surf)

    if (p%write_temp_base .eq. 1) &
      print "('TEMP_BASE (max, mean, min) [C]       : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%temp_base), meanval_intr(s%temp_base), minval_intr(s%temp_base)

    if (p%write_temp_ice .eq. 1) &
      print "('TEMP_ICE (max, mean, min) [C]        : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%temp_ice), meanval_intr(s%temp_ice), minval_intr(s%temp_ice)

    if (p%write_precip .eq. 1) &
      print "('PRECIP (max, mean, min) [m/a]        : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%precip), meanval_intr(s%precip), minval_intr(s%precip)

    if (p%write_runoff .eq. 1) &
      print "('RUNOFF (max, mean, min) [m/a]        : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%runoff), meanval_intr(s%runoff), minval_intr(s%runoff)

    if (p%write_ice_q_surf .eq. 1) &
      print "('ICE_Q_SURF (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_q_surf), meanval_intr(s%ice_q_surf), minval_intr(s%ice_q_surf)
    
    if (p%write_ice_h .eq. 1) &
      print "('ICE_H (max, mean, min) [m]           : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_h), meanval_intr(s%ice_h), minval_intr(s%ice_h)

    if (p%write_ice_h_dot .eq. 1) &
      print "('ICE_H_DOT (max, mean, min) [m/a]     : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_h_dot), meanval_intr(s%ice_h_dot), minval_intr(s%ice_h_dot)

    if (p%write_ice_uv_defm .eq. 1) &
      print "('ICE_U_DEFM (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_u_defm), meanval_intr(s%ice_u_defm), minval_intr(s%ice_u_defm)

    if (p%write_ice_uv_defm .eq. 1) &
      print "('ICE_V_DEFM (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_v_defm), meanval_intr(s%ice_v_defm), minval_intr(s%ice_v_defm)

    if (p%write_ice_uv_slid .eq. 1) &
      print "('ICE_U_SLID (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_u_slid), meanval_intr(s%ice_u_slid), minval_intr(s%ice_u_slid)

    if (p%write_ice_uv_slid .eq. 1) &
      print "('ICE_V_SLID (max, mean, min) [m/a]    : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_v_slid), meanval_intr(s%ice_v_slid), minval_intr(s%ice_v_slid)

    if (p%write_ice_h_soln .eq. 1) &
      print "('ICE_H_SOLN (max, mean, min) [m]      : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_h_soln), meanval_intr(s%ice_h_soln), minval_intr(s%ice_h_soln)

    if (p%write_ice_a_defm .eq. 1) &
      print "('ICE_A_DEFM (max, mean, min) [1/Pa3/a]: ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_a_defm), meanval_intr(s%ice_a_defm), minval_intr(s%ice_a_defm)

    if (p%write_ice_a_slid .eq. 1) &
      print "('ICE_A_SLID (max, mean, min) [1/Pa/a] : ', EN12.3, ', ', EN12.3, ', ', EN12.3)", &
        maxval_intr(s%ice_a_slid), meanval_intr(s%ice_a_slid), minval_intr(s%ice_a_slid)

    print *, ''

  end subroutine write_status

end module io
