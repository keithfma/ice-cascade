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
use climate_mod, only: climate_type
use ice_mod, only: ice_type
use netcdf

implicit none
private
public :: io_type


  ! ---------------------------------------------------------------------------
  ! TYPE: input/output vars and procedures
  ! ---------------------------------------------------------------------------
  type io_type
    character(len=100) :: name_in ! input file name
    character(len=100) :: name_out ! output file name
    !integer :: n_step ! counter for current output step
    real(rp) :: time_step ! interval btw outputs, [a]
    !logical :: write_topo 
    !logical :: write_topo_dot_ice
    !logical :: write_temp_surf 
    !logical :: write_temp_base 
    !logical :: write_temp_ice 
    !logical :: write_precip 
    !logical :: write_runoff 
    !logical :: write_ice_q_surf 
    !logical :: write_ice_h 
    !logical :: write_ice_h_dot
    !logical :: write_ice_uvd 
    !logical :: write_ice_uvs 
    !logical :: write_ice_h_soln
  contains
    procedure, pass :: read_param ! read parameters from the input file
    procedure, pass :: read_var ! read initial values 
    !procedure, pass :: create_output ! create output file
    !procedure, nopass :: write_status ! print model status to screen
    !procedure, pass :: write_output_step ! write step
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
  subroutine read_param(io, prm, cli, ice)

    class(io_type), intent(out) :: io
    type(param_type), intent(out) :: prm
    type(climate_type), intent(out) :: cli
    type(ice_type), intent(out) :: ice
    
    integer :: ncid, e, n

    ! Get input arguments
    select case (command_argument_count())
      case (2)
      	call get_command_argument(1, io%name_in) 
      	call get_command_argument(2, io%name_out) 
      case default
      	print*,'ICE-CASCADE expects exactly 2 input arguments'
      	stop -1
    end select

    ! Open file
    e = nf90_open(io%name_in, nf90_nowrite, ncid)
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
    
    e = nf90_get_att(ncid, nf90_global, 'time_step_write__a', io%time_step)
    call err_req('read_param: time_step_write__a: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'climate_name', cli%name)
    call err_req('read_param: climate_name: ', e)
    
    e = nf90_inquire_attribute(ncid, nf90_global, 'climate_param__var', len = n)
    call err_req('read_param: climate_param__var: ', e)
    allocate(cli%param(n))
    e = nf90_get_att(ncid, nf90_global, 'climate_param__var', cli%param)
    call err_req('read_param: climate_param__var: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'ice_name', ice%name)
    call err_req('read_param: ice_name: ', e)
    
    e = nf90_inquire_attribute(ncid, nf90_global, 'ice_param__var', len = n)
    call err_req('read_param: ice_param__var: ', e)
    allocate(ice%param(n))
    e = nf90_get_att(ncid, nf90_global, 'ice_param__var', ice%param)
    call err_req('read_param: ice_param__var: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'ice_name_ebc', ice%name_ebc)
    call err_req('read_param: ice_name_ebc: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'ice_name_wbc', ice%name_wbc)
    call err_req('read_param: ice_name_wbc: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'ice_name_nbc', ice%name_nbc)
    call err_req('read_param: ice_name_nbc: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'ice_name_sbc', ice%name_sbc)
    call err_req('read_param: ice_name_sbc: ', e)
    
    e = nf90_get_att(ncid, nf90_global, 'ice_name_soln', ice%name_soln)
    call err_req('read_param: ice_name_soln: ', e)
    
    e = nf90_inquire_attribute(ncid, nf90_global, 'ice_param_soln__var', len = n)
    call err_req('read_param: ice_param_soln__var: ', e)
    allocate(ice%param_soln(n))
    e = nf90_get_att(ncid, nf90_global, 'ice_param_soln__var', ice%param_soln)
    call err_req('read_param: ice_param_soln__var: ', e)

    ! Close file
    e = nf90_close(ncid)
    call err_req('read_param: close file: ', e)

  end subroutine read_param


  !----------------------------------------------------------------------------
  ! SUB: Read initial variables from netcdf file
  !   unspecified variables retain the default value (0)
  ! ---------------------------------------------------------------------------
  subroutine read_var(io, prm, sta)

    class(io_type), intent(in) :: io
    type(param_type), intent(in) :: prm
    type(state_type), intent(inout) :: sta

    integer :: e, ncid, varid
    
    ! Open file
    e = nf90_open(io%name_in, nf90_nowrite, ncid)
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

    !! template for additional vars
    !e = nf90_inq_varid(ncid, 'NAME', varid)
    !call err_opt('read_vars: NAME: ', e)
    !e = nf90_get_var(ncid, varid, sta%NAME(2:prm%nx+1, 2:prm%ny+1))
    !call err_opt('read_vars: NAME: ', e)

    ! Close file
    e = nf90_close(ncid)
    call err_req('read_param: close file: ', e)

  end subroutine read_var




    




!  ! ---------------------------------------------------------------------------
!  ! OLD: Read input parameters from file
!  ! ---------------------------------------------------------------------------
!  subroutine read_param(io, s, c, g)
!
!    class(io_type), intent(out) :: io
!    type(state_type), intent(out) :: s
!    type(climate_type), intent(out) :: c
!    type(ice_type), intent(out) :: g
!
!    character(len=100) :: infile, line
!    integer :: msg
!
!    ! Get input file name from command line
!    select case (command_argument_count())
!      case (1)
!      	call get_command_argument(1,infile) 
!      case default
!      	print*,'ICE-CASCADE expects exactly 1 input argument'
!      	stop -1
!    end select
!    
!    ! Sanitize input file (drop comments and empty lines)
!    open(54, file = trim(infile), status = 'old', iostat = msg)
!    if (msg .ne. 0) then
!    	print*, 'The input file ', trim(infile), ' does not exist, exiting.'
!    	stop
!    end if	
!    open (55, status = 'scratch')
!    do while (msg .ge. 0)
!    	read (54, '(a)', iostat = msg) line
!    	if ((line(1:1) .ne. '$') .and. (line(1:1) .ne. ' ')) write (55, '(a)') line		
!    enddo 
!    close (54)
!    rewind (55)
!    
!    ! Read input parameters
!    read(55, *) s%time_start
!    read(55, *) s%time_finish
!    read(55, *) s%time_step
!    read(55, *) s%rhoi
!    read(55, *) s%nx	
!    read(55, *) s%ny
!    read(55, *) s%lx
!    read(55, *) s%ly	
!    read(55, *) io%name_topo 
!    read(55, *) io%name_ice_h
!    read(55, *) c%on_temp_surf 
!    read(55, *) c%name_temp_surf 
!    read(55, *) c%param_temp_surf(:) 
!    read(55, *) c%on_precip 
!    read(55, *) c%name_precip
!    read(55, *) c%param_precip(:) 
!    read(55, *) c%on_ice_q_surf 
!    read(55, *) c%name_ice_q_surf 
!    read(55, *) c%param_ice_q_surf(:) 
!    read(55, *) c%on_runoff 
!    read(55, *) c%name_runoff 
!    read(55, *) c%param_runoff(:) 
!    read(55, *) g%on
!    read(55, *) g%A0
!    read(55, *) g%As0
!    read(55, *) g%name_flow
!    read(55, *) g%name_nbc
!    read(55, *) g%name_sbc
!    read(55, *) g%name_ebc
!    read(55, *) g%name_wbc
!    read(55, *) g%name_soln
!    read(55, *) g%param_soln(:)
!    read(55, *) io%name_out	
!    read(55, *) io%time_step
!    read(55, *) io%write_topo
!    read(55, *) io%write_temp_surf
!    read(55, *) io%write_precip
!    read(55, *) io%write_runoff
!    read(55, *) io%write_ice_q_surf
!    read(55, *) io%write_ice_h
!    read(55, *) io%write_ice_h_dot
!    read(55, *) io%write_ice_uvd
!    read(55, *) io%write_ice_uvs
!    read(55, *) io%write_temp_base
!    read(55, *) io%write_temp_ice
!    read(55, *) io%write_topo_dot_ice
!    read(55, *) io%write_ice_h_soln
!
!    ! Check for sane values
!    ! positive grid dimensions 
!    if ((s%nx .le. 0) .or. (s%ny .le. 0)) then
!      print *, 'Invalid grid description: num grid points must be positive integers.'
!      stop -1
!    end if
!    if ((s%lx .le. 0.0_rp) .or. (s%ly .le. 0.0_rp)) then
!      print *, 'Invalid grid description: grid dimensions must be positive.'
!      stop -1
!    end if
!
!    ! positive densities
!    if (s%rhoi .le. 0.0_rp) then
!      print *, 'Invalid physical constant: density must be positive.'
!      stop -1
!    end if
!
!    ! non-zero timestep 
!    if (s%time_step .eq. 0.0_rp) then
!      print *, 'Invalid time parameters: time step cannot be zero.'
!      stop -1
!    end if
!
!    ! forward model, start before finish
!    if ((s%time_step .gt. 0.0_rp) .and. (s%time_start .ge. s%time_finish))  then
!      print *, 'Invalid time parameters: start must be before finish for & 
!               &forward models.'
!      stop -1
!    end if
!
!    ! reverse model, finish before start
!    if ((s%time_step .lt. 0.0_rp) .and. (s%time_start .le. s%time_finish)) then
!      print *, 'Invalid time parameters: start must be after finish for &
!               &reverse models.'
!      stop -1
!    end if
!
!    ! output interval matches time step
!    if (mod(io%time_step, s%time_step) .ne. 0.0_rp) then
!      print *, 'Invalid time parameters: output interval must be a multiple &
!               &of the time step.'
!      stop -1
!    end if
!/filename
!
!    ! positive ice deformation parameters
!    if (g%A0 .lt. 0.0_rp) then
!      print *, 'Invalid ice model parameters: ice deformation prefactor must &
!               &be positive'
!      stop -1
!    end if
!    ! TO-DO: add ice defm exponent when polythermal is implemented
!
!    ! positive ice sliding parameter
!    if (g%As0 .lt. 0.0_rp) then
!      print *, 'Invalid ice model parameters: ice sliding coefficient must &
!               &be positive'
!      stop -1
!    end if
!
!  end subroutine read_param
!
!
!  ! ---------------------------------------------------------------------------
!  ! SUB: read the initial values for state variables
!  ! TO-DO: add initial ice thickness
!  ! ---------------------------------------------------------------------------
!  subroutine read_initial_vals(io, s, g)
!
!    class(io_type), intent(in) :: io 
!    type(state_type), intent(inout) :: s
!    type(ice_type), intent(in) :: g
!
!    ! initial topography
!    select case(io%name_topo)
!    case('zero')
!      s%topo = 0.0_rp
!    case default
!      call read_array_nc(io%name_topo, s%nx, s%ny, s%topo)
!    end select
!
!    ! initial ice thickness
!    select case(io%name_ice_h)
!    case('zero')
!      s%ice_h = 0.0_rp
!    case('exact')
!      if (g%on_soln) then
!        call g%solve(s)
!        s%ice_h = s%ice_h_soln
!      else
!        print *, 'Invalid initial ice thickness: exact solution is not set'
!        stop -1
!      end if
!    case default
!      call read_array_nc(io%name_ice_h, s%nx, s%ny, s%ice_h)
!    end select
!
!  end subroutine read_initial_vals
!
!
!  ! --------------------------------------------------------------------------- 
!  ! SUB: read 2D array from GMT grd format netcdf
!  ! --------------------------------------------------------------------------- 
!  subroutine read_array_nc(filename, nx, ny, array)
!    
!    character(len=*), intent(in) :: filename
!    integer, intent(in) :: nx, ny
!    real(rp), intent(out) :: array(:,:)
!
!    integer :: ndim, nvar, n1, n2, ind, i, var_ndim
!    integer :: msg, id_file
!
!    ! open file
!	  msg = nf90_open(trim(filename), nf90_nowrite, id_file)
!
!    ! check dimensions
!	  msg = nf90_inquire(id_file, ndim, nvar)	
!	  if (ndim .ne. 2) then
!		  print *,'Invalid input data: netcdf file should have only two dimensions'
!      stop -1
!    end if	
!		msg = nf90_inquire_dimension(id_file, 1, len = n1)	
!		if (n1 .ne. nx) then
!      print *,'Invalid input data: data does not match the given dimensions (nx)'
!      stop -1
!    end if
!		msg = nf90_inquire_dimension(id_file, 2, len = n2)	
!		if (n2 .ne. ny) then
!      print *,'Invalid input data: data does not match the given dimensions (ny)'
!      stop -1
!    end if
!    
!    ! identify array variable 
!    ind = -1
!    do i = 1, nvar
!			msg = nf90_inquire_variable(id_file, i, ndims = var_ndim)
!		  if ((var_ndim .eq. 2) .and. (ind .ne. -1)) then
!				print*,'Invalid input data: netcdf file should only have one 2D variable'
!        stop -1
!      end if
!      if (var_ndim .eq. 2) ind = i
!  	end do	
!
!    ! read array to interior points
!	  msg = nf90_get_var(id_file, ind, array(2:nx+1, 2:ny+1))
!    
!    ! close file
!    msg = nf90_close(id_file)
!
!  end subroutine read_array_nc
!
!
!  ! ---------------------------------------------------------------------------
!  ! SUB: create output netcdf file
!  ! ---------------------------------------------------------------------------
!  subroutine create_output(io, s, c, g)
!
!    class(io_type), intent(in) :: io
!    type(state_type), intent(in) :: s
!    type(climate_type), intent(in) :: c
!    type(ice_type), intent(in) :: g
!
!    logical :: shuf
!    integer :: fchunk(3), i, j, defLvl, msg, id_file, id_dim_x, id_dim_y, &
!               id_dim_t, id_var
!
!    ! define compression and chunking parameters 
!    defLvl = 1 ! compression, 0 = none, 9 = max, best value is 1
!    fchunk = [s%nx, s%ny, 1]
!    shuf = .true.
!
!    ! create new file
!    msg = nf90_create(io%name_out, nf90_netcdf4, id_file)
!
!    ! write parameters as global attributes
!    msg = nf90_put_att(id_file, nf90_global, 'time_start_time__a', s%time_start)
!    msg = nf90_put_att(id_file, nf90_global, 'time_finish_time__a', s%time_finish)
!    msg = nf90_put_att(id_file, nf90_global, 'time_step__a', s%time_step)
!    msg = nf90_put_att(id_file, nf90_global, 'time_step_output__a', io%time_step)
!    msg = nf90_put_att(id_file, nf90_global, 'density_ice__kg1m-3', s%rhoi)
!    msg = nf90_put_att(id_file, nf90_global, 'grid_nx__1', s%nx)
!    msg = nf90_put_att(id_file, nf90_global, 'grid_ny__1', s%ny)
!    msg = nf90_put_att(id_file, nf90_global, 'grid_dx__m', s%dx)
!    msg = nf90_put_att(id_file, nf90_global, 'grid_dy__m', s%dy)
!    msg = nf90_put_att(id_file, nf90_global, 'topo_initial__name', io%name_topo)
!    msg = nf90_put_att(id_file, nf90_global, 'ice_h_initial__name', io%name_ice_h)
!    msg = nf90_put_att(id_file, nf90_global, 'climate_temp_surf_on__tf', merge(1, 0, c%on_temp_surf))
!    if (c%on_temp_surf) then
!      msg = nf90_put_att(id_file, nf90_global, 'climate_temp_surf__name', c%name_temp_surf)
!      msg = nf90_put_att(id_file, nf90_global, 'climate_temp_param__various', c%param_temp_surf)
!    end if
!    msg = nf90_put_att(id_file, nf90_global, 'climate_precip_on__tf', merge(1, 0, c%on_precip))
!    if (c%on_precip) then
!      msg = nf90_put_att(id_file, nf90_global, 'climate_precip__name', c%name_precip)
!      msg = nf90_put_att(id_file, nf90_global, 'climate_precip_param__various', c%param_precip)
!    end if
!    msg = nf90_put_att(id_file, nf90_global, 'climate_ice_q_surf_on__tf', merge(1, 0, c%on_ice_q_surf))
!    if (c%on_ice_q_surf) then
!      msg = nf90_put_att(id_file, nf90_global, 'climate_ice_q_surf__name', c%name_ice_q_surf)
!      msg = nf90_put_att(id_file, nf90_global, 'climate_ice_q_surf_param__various', c%param_ice_q_surf)
!    end if
!    msg = nf90_put_att(id_file, nf90_global, 'climate_runoff_on__tf', merge(1, 0, c%on_runoff))
!    if (c%on_runoff) then
!      msg = nf90_put_att(id_file, nf90_global, 'climate_runoff__name', c%name_runoff)
!      msg = nf90_put_att(id_file, nf90_global, 'climate_runoff_param__various', c%param_runoff)
!    end if
!    msg = nf90_put_att(id_file, nf90_global, 'ice_on__tf', merge(1, 0, g%on))
!    if (g%on) then
!      msg = nf90_put_att(id_file, nf90_global, 'ice_defm_prefactor__Pa-3_a-1', g%A0)
!      msg = nf90_put_att(id_file, nf90_global, 'ice_sliding_coeff__TBD', g%As0) ! TO-DO: add units once I know them 
!      msg = nf90_put_att(id_file, nf90_global, 'ice_bc_north__name', g%name_nbc)
!      msg = nf90_put_att(id_file, nf90_global, 'ice_bc_south__name', g%name_sbc)
!      msg = nf90_put_att(id_file, nf90_global, 'ice_bc_east__name', g%name_ebc)
!      msg = nf90_put_att(id_file, nf90_global, 'ice_bc_west__name', g%name_wbc)
!      msg = nf90_put_att(id_file, nf90_global, 'ice_flow_method__name', g%name_flow)
!    end if
!    msg = nf90_put_att(id_file, nf90_global, 'ice_on_soln__tf', merge(1, 0, g%on_soln))
!    if(g%on_soln) then
!      msg = nf90_put_att(id_file, nf90_global, 'ice_soln__name', g%name_soln)
!    end if
!
!    ! define dimensions
!    msg = nf90_def_dim(id_file, 'x', s%nx, id_dim_x) 
!    msg = nf90_def_dim(id_file, 'y', s%ny, id_dim_y) 
!    msg = nf90_def_dim(id_file, 't', nf90_unlimited, id_dim_t)
!
!    ! create coordinate variables
!    msg = nf90_def_var(id_file, 't', rp_nc, id_dim_t, id_var)
!    msg = nf90_put_att(id_file, id_var, 'long_name', 'model_time')
!    msg = nf90_put_att(id_file, id_var, 'units', 'a')
!
!    msg = nf90_def_var(id_file, 'x', rp_nc, id_dim_x, id_var)
!    msg = nf90_put_att(id_file, id_var, 'long_name', 'x_coord')
!    msg = nf90_put_att(id_file, id_var, 'units', 'm')
!    
!    msg = nf90_def_var(id_file, 'y', rp_nc, id_dim_y, id_var)
!    msg = nf90_put_att(id_file, id_var, 'long_name', 'y_coord')
!    msg = nf90_put_att(id_file, id_var, 'units', 'm')
!    
!    ! create variables
!    if (io%write_topo) then
!     	msg = nf90_def_var(id_file, 'topo', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'topography')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm')
!    end if
!    if (io%write_temp_surf) then
!     	msg = nf90_def_var(id_file, 'temp_surf', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'surface_temperature')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'C')
!    end if
!    if (io%write_precip) then
!     	msg = nf90_def_var(id_file, 'precip', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'precipitation_rate')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm_water/a')
!    end if
!    if (io%write_ice_q_surf) then
!     	msg = nf90_def_var(id_file, 'ice_q_surf', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'surface_ice_flux')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm_ice/a')
!    end if
!    if (io%write_runoff) then
!     	msg = nf90_def_var(id_file, 'runoff', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'runoff rate')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm_water/a')
!    end if
!    if (io%write_topo_dot_ice) then
!     	msg = nf90_def_var(id_file, 'topo_dot_ice', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'topo_rate_of_change_from_ice')
!     	msg = nf90_put_att(id_file, id_var, 'units', '')
!    end if
!    if (io%write_temp_base) then
!     	msg = nf90_def_var(id_file, 'temp_base', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_basal_temperature')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'C')
!    end if
!    if (io%write_temp_ice) then
!     	msg = nf90_def_var(id_file, 'temp_ice', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_mean_temperature')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'C')
!    end if
!    if (io%write_ice_h) then
!     	msg = nf90_def_var(id_file, 'ice_h', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_thickness')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm')
!    end if
!    if (io%write_ice_h_dot) then
!     	msg = nf90_def_var(id_file, 'ice_h_dot', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_thickness_rate_of_change')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm/a')
!    end if
!    if (io%write_ice_uvd) then
!     	msg = nf90_def_var(id_file, 'ice_ud', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_deformation_velocity_x')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm/a')
!
!     	msg = nf90_def_var(id_file, 'ice_vd', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_deformation_velocity_y')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm/a')
!    end if
!    if (io%write_ice_uvs) then
!     	msg = nf90_def_var(id_file, 'ice_us', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_sliding_velocity_x')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm/a')
!
!     	msg = nf90_def_var(id_file, 'ice_vs', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_sliding_velocity_y')
!     	msg = nf90_put_att(id_file, id_var, 'units', 'm/a')
!    end if
!    if (io%write_ice_h_soln) then
!     	msg = nf90_def_var(id_file, 'ice_h_soln', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_thickness_solution')
!     	msg = nf90_put_att(id_file, id_var, 'units', '')
!    end if
!    !if (io%write_) then
!    ! 	msg = nf90_def_var(id_file, '', rp_nc, [id_dim_x, id_dim_y, id_dim_t],  &
!    !    id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
!    ! 	msg = nf90_put_att(id_file, id_var, 'long_name', '')
!    ! 	msg = nf90_put_att(id_file, id_var, 'units', '')
!    !end if
!
!    ! exit definition mode
!    msg = nf90_enddef(id_file) 
!
!    ! populate dimension variables
!    msg = nf90_inq_varid(id_file, 'x', id_var)
!    msg = nf90_put_var(id_file, id_var, s%x(2:s%nx+1))
!
!    msg = nf90_inq_varid(id_file, 'y', id_var)
!    msg = nf90_put_var(id_file, id_var, s%y(2:s%ny+1))
!
!    ! close file
!    msg = nf90_close(id_file)
!
!  end subroutine create_output
!
!
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
!  ! ---------------------------------------------------------------------------
!  ! SUB: Write output step
!  ! ---------------------------------------------------------------------------
!  subroutine write_output_step(io, s)
!    
!    class(io_type), intent(inout) :: io
!    type(state_type), intent(inout) :: s
!
!    integer :: i0, i1, j0, j1, msg, id_file, id_var
!
!    ! increment step counter
!    io%n_step = io%n_step+1
!
!    ! define limits of interior points, for convenience
!    i0 = 2; i1 = s%nx+1
!    j0 = 2; j1 = s%ny+1
!
!    ! open file
!    msg = nf90_open(io%name_out, nf90_write, id_file)
!
!    ! write time data
!    msg = nf90_inq_varid(id_file, 't', id_var)
!    msg = nf90_put_var(id_file, id_var, real(s%time_now, rp), [io%n_step] )
!
!    ! write arrays 
!    if (io%write_topo) then
!      msg = nf90_inq_varid(id_file, 'topo', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%topo(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!    end if
!    if (io%write_topo_dot_ice) then
!      msg = nf90_inq_varid(id_file, 'topo_dot_ice', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%topo_dot_ice(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!    end if
!    if (io%write_temp_surf) then
!      msg = nf90_inq_varid(id_file, 'temp_surf', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%temp_surf(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!    end if
!    if (io%write_temp_ice) then
!      msg = nf90_inq_varid(id_file, 'temp_ice', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%temp_ice(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!    end if
!    if (io%write_temp_base) then
!      msg = nf90_inq_varid(id_file, 'temp_base', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%temp_base(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!    end if
!    if (io%write_precip) then
!      msg = nf90_inq_varid(id_file, 'precip', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%precip(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!    end if
!    if (io%write_runoff) then
!      msg = nf90_inq_varid(id_file, 'runoff', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%runoff(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!    end if
!    if (io%write_ice_q_surf) then
!      msg = nf90_inq_varid(id_file, 'ice_q_surf', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%ice_q_surf(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!    end if
!    if (io%write_ice_h) then
!      msg = nf90_inq_varid(id_file, 'ice_h', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%ice_h(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!    end if
!    if (io%write_ice_h_dot) then
!      msg = nf90_inq_varid(id_file, 'ice_h_dot', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%ice_h_dot(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!    end if
!    if (io%write_ice_h_soln) then
!      msg = nf90_inq_varid(id_file, 'ice_h_soln', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%ice_h_soln(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!    end if
!    if (io%write_ice_uvd) then
!      msg = nf90_inq_varid(id_file, 'ice_ud', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%ice_ud(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!      msg = nf90_inq_varid(id_file, 'ice_vd', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%ice_vd(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!    end if
!    if (io%write_ice_uvs) then
!      msg = nf90_inq_varid(id_file, 'ice_us', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%ice_us(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!      msg = nf90_inq_varid(id_file, 'ice_vs', id_var)
!      msg = nf90_put_var(id_file, id_var, real(s%ice_vs(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!    end if
!    !if (io%write_) then
!    !  msg = nf90_inq_varid(id_file, '', id_var)
!    !  msg = nf90_put_var(id_file, id_var, real(s%(i0:i1, j0:j1), rp), [1, 1, io%n_step])
!    !end if
!
!    ! close file
!    msg = nf90_close(id_file)
!
!  end subroutine write_output_step

end module io_mod
