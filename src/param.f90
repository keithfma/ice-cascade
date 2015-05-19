! =============================================================================
! Shared parameters and state variables for ice-cascade. 
!
! Public:
!   param_type: derived type, all model parameters
!   init_param: procedure, check for sane parameters
! 
! Private: none
!
! ============================================================================

module param

use kinds, only: rp

implicit none
private
public :: param_type, init_param

  ! --------------------------------------------------------------------------- 
  ! TYPE: shared parameters
  ! ---------------------------------------------------------------------------
  type param_type
    character(len=500) :: input_file ! input file name
    character(len=500) :: output_file ! output file name
    integer :: nx ! num grid points in x-dir, [1]
    integer :: ny ! num grid points in y-dir, [1]
    real(rp) :: lx ! grid dim in x-dir, [m]
    real(rp) :: ly ! grid dim in y-dir, [m]
    real(rp) :: dx ! grid spacing in x-dir, [m]
    real(rp) :: dy ! grid spacing in y-dir, [m]
    real(rp) :: rhoi ! density of glacial ice, [kg/m3]
    real(rp) :: grav ! acceleration of gravity, [m/s2]
    real(rp) :: time_start ! start time [a]
    real(rp) :: time_finish ! finish time [a]
    real(rp) :: time_step ! time step [a]
    real(rp), allocatable :: time_write(:) ! output times, [a]
    character(len=500) :: climate_name ! climate method name
    real(rp), allocatable :: climate_param(:) ! climate model parameters, [various]
    character(len=500) :: ice_name ! ice method name
    character(len=500) :: ice_bc_name ! ice BC names (comma-delimited)
    real(rp), allocatable :: ice_param(:) ! ice model parameters, [various]
    character(len=100) :: ice_soln_name ! ice model exact solution name
    real(rp), allocatable :: ice_soln_param(:) ! ice solution parameters, [various]
    logical :: write_topo ! write flag for state var
    logical :: write_topo_dot_ice ! ' '
    logical :: write_surf
    logical :: write_temp_surf 
    logical :: write_temp_base 
    logical :: write_temp_ice 
    logical :: write_precip 
    logical :: write_runoff 
    logical :: write_ice_q_surf 
    logical :: write_ice_h 
    logical :: write_ice_h_dot
    logical :: write_ice_uvd 
    logical :: write_ice_uvs 
    logical :: write_ice_h_soln
  end type param_type


contains


  ! ---------------------------------------------------------------------------
  ! SUB: check for sane parameters and add ghost points
  ! ---------------------------------------------------------------------------
  subroutine init_param(p)

    type(param_type), intent(inout) :: p

    integer :: i

    ! positive grid dimensions
    if (p%nx .le. 0) then
      print *, 'Invalid parameter: nx must be positive.'
      stop 'Stopped.'
    end if
    if (p%ny .le. 0) then
      print *, 'Invalid parameter: ny must be positive.'
      stop 'Stopped.'
    end if
    if (p%lx .le. 0) then
      print *, 'Invalid parameter: lx must be positive.'
      stop 'Stopped.'
    end if
    if (p%ly .le. 0) then
      print *, 'Invalid parameter: ly must be positive.'
      stop 'Stopped.'
    end if

    ! positive grid spacing
    if (p%dx .le. 0.0_rp) then
      print *, 'Invalid parameter: dx must be positive.'
      stop 'Stopped.'
    end if
    if (p%dy .le. 0.0_rp) then
      print *, 'Invalid parameter: dy must be positive.'
      stop 'Stopped.'
    end if

    ! positive densities
    if (p%rhoi .le. 0.0_rp) then
      print *, 'Invalid parameter: rhoi must be positive.'
      stop 'Stopped.'
    end if

    ! positive gravity
    if (p%grav .le. 0.0_rp) then
      print *, 'Invalid parameter: grav must be positive.'
      stop 'Stopped.'
    end if

    ! non-zero timestep
    if (p%time_step .eq. 0.0_rp) then
      print *, 'Invalid parameter: time_step must be non-zero.'
      stop 'Stopped.'
    end if

    ! forward model: start before finish 
    if ((p%time_step .gt. 0.0_rp) .and. (p%time_start .ge. p%time_finish)) then
      print *, 'Invalid parameters: start_time must be before finish_time for forward models.'
      stop 'Stopped.'
    end if

    ! reverse model: start after finish
    if ((p%time_step .lt. 0.0_rp) .and. (p%time_start .le. p%time_finish)) then
      print *, 'Invalid parameters: start_time must be after finish_time for reverse models.'
      stop 'Stopped.'
    end if

    ! model duration is an integer multiple of time_step
    if (mod(p%time_start-p%time_finish, p%time_step) .gt. epsilon(0.0_rp)) then
      print *, 'Invalid parameters: model duration must be an integer number of time steps.'
      stop 'Stopped.'
    end if

    ! write steps must be monotonic 
    if (size(p%time_write) .gt. 1) then
      do i = 1, size(p%time_write)-1
        if ((p%time_step .gt. 0.0_rp) .and. &
            (p%time_write(i+1) .le. p%time_write(i))) then
          print *, 'Invalid parameters: time_write must be monotonically &
                   &increasing for forward models'
          stop
        end if
        if ((p%time_step .lt. 0.0_rp) .and. &
            (p%time_write(i+1) .ge. p%time_write(i))) then
          print *, 'Invalid parameters: time_write must be monotonically &
                   &decreasing for reverse models'
          stop
        end if
      end do
    end if
   
    ! write steps must all be between start and finish
    if ((minval(p%time_write) .lt. min(p%time_start, p%time_finish)) .or. &
        (maxval(p%time_write) .gt. max(p%time_start, p%time_finish))) then
      print *, 'Invalid parameters: write steps must all be between start &
                &and finish.'
      stop
    end if

    ! computational grid has 1 ghost-point on all boundaries
    p%nx = p%nx+2
    p%ny = p%ny+2

  end subroutine init_param

end module param
