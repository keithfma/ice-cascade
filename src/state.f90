! =============================================================================
! Shared parameters and state variables for ice-cascade. 
!
! Public:
!   state_type: derived type, model state variables
!   init_state: procedure, allocate and initialize all state variables
!
! Private: none
!
! ============================================================================

module state

use kinds, only: rp
use param, only: param_type

implicit none
private
public :: state_type, init_state

  ! --------------------------------------------------------------------------- 
  type state_type
    real(rp) :: now ! current model time, [a]
    real(rp) :: step ! current model time step, [a]
    real(rp), allocatable :: x(:) ! x coordinate vector, [m]
    real(rp), allocatable :: y(:) ! y coordinate vector, [m]
    real(rp), allocatable :: topo(:,:) ! bedrock elevation, [m above sea level]
    real(rp), allocatable :: topo_dot_ice(:,:) ! topography rate-of-change due to glaciers, [m/a]
    real(rp), allocatable :: surf(:,:) ! surface elevation (topo+ice_h), [m above sea level]
    real(rp), allocatable :: temp_surf(:,:) ! temp at ice/bedrock surface, [C]
    real(rp), allocatable :: temp_ice(:,:) ! mean ice temperature, [C]
    real(rp), allocatable :: temp_base(:,:) ! temp at bedrock surface, [C]
    real(rp), allocatable :: precip(:,:) ! precipitation rate, [m_water/a]
    real(rp), allocatable :: runoff(:,:) ! runoff rate, [m_water/a]
    real(rp), allocatable :: ice_q_surf(:,:) ! surface ice flux, [m_ice/a]
    real(rp), allocatable :: ice_h(:,:) ! ice thickness, [m]
    real(rp), allocatable :: ice_h_dot(:,:) ! ice thickness rate-of-change, [m/a]
    real(rp), allocatable :: ice_h_soln(:,:) ! exact solution for ice thickness, [m]
    real(rp), allocatable :: ice_ud(:,:) ! ice deformation velocity, x-dir, [m/a]
    real(rp), allocatable :: ice_vd(:,:) ! ice deformation velocity, y-dir, [m/a]
    real(rp), allocatable :: ice_us(:,:) ! ice sliding velocity, x-dir, [m/a]
    real(rp), allocatable :: ice_vs(:,:) ! ice sliding velocity, y-dir, [m/a]
    real(rp), allocatable :: ice_a(:,:) ! ice deformation coeff, [Pa^-3 a^-1]
    real(rp), allocatable :: ice_as(:,:) ! ice sliding coeff, [CHECK UNITS]
  end type state_type
  !
  ! ABOUT: shared state variables
  ! ---------------------------------------------------------------------------


contains


  ! ---------------------------------------------------------------------------
  subroutine init_state(p, s)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: allocate and initialize variables
  ! ---------------------------------------------------------------------------

    ! allocate arrays
    allocate(s%x(p%nx))
    allocate(s%y(p%ny))
    allocate(s%topo(p%nx, p%ny))
    allocate(s%topo_dot_ice(p%nx, p%ny))
    allocate(s%surf(p%nx, p%ny))
    allocate(s%temp_surf(p%nx, p%ny))
    allocate(s%temp_ice(p%nx, p%ny))
    allocate(s%temp_base(p%nx, p%ny))
    allocate(s%precip(p%nx, p%ny))
    allocate(s%runoff(p%nx, p%ny))
    allocate(s%ice_q_surf(p%nx, p%ny))
    allocate(s%ice_h(p%nx, p%ny))
    allocate(s%ice_h_dot(p%nx, p%ny))
    allocate(s%ice_h_soln(p%nx, p%ny))
    allocate(s%ice_ud(p%nx, p%ny))
    allocate(s%ice_vd(p%nx, p%ny))
    allocate(s%ice_us(p%nx, p%ny))
    allocate(s%ice_vs(p%nx, p%ny))
    allocate(s%ice_vs(p%nx, p%ny))
    allocate(s%ice_a(p%nx, p%ny))
    allocate(s%ice_as(p%nx, p%ny))

    ! set initial values (some are overwritten by read_vars)
    s%now = p%time_start
    s%step = p%time_step
    s%x = 0.0_rp
    s%y = 0.0_rp
    s%topo = 0.0_rp
    s%surf = 0.0_rp
    s%topo_dot_ice = 0.0_rp
    s%temp_surf = 0.0_rp
    s%temp_ice = 0.0_rp
    s%temp_base = 0.0_rp
    s%precip = 0.0_rp
    s%runoff = 0.0_rp
    s%ice_q_surf = 0.0_rp
    s%ice_h = 0.0_rp
    s%ice_h_dot = 0.0_rp
    s%ice_h_soln = 0.0_rp
    s%ice_ud = 0.0_rp
    s%ice_vd = 0.0_rp
    s%ice_us = 0.0_rp
    s%ice_vs = 0.0_rp
    s%ice_a = 0.0_rp
    s%ice_as = 0.0_rp

  end subroutine init_state

end module state 
