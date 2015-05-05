! =============================================================================
! Shared parameters and state variables for ice-cascade. 
!
! Contains:
!   type state_type (public)
!
! ============================================================================

module state_mod

use kinds_mod, only: rp

implicit none
private
public :: state_type

  ! --------------------------------------------------------------------------- 
  ! TYPE: shared state variables
  ! ---------------------------------------------------------------------------
  type state_type
    real(rp), allocatable :: x(:) ! x coordinate vector, [m]
    real(rp), allocatable :: y(:) ! y coordinate vector, [m]
    real(rp), allocatable :: topo(:,:) ! bedrock elevation, [m above sea level]
    real(rp), allocatable :: topo_dot_ice(:,:) ! topography rate-of-change due to glaciers, [m/a]
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
  contains
    procedure, pass :: alloc ! allocate all dynamic arrays 
  end type state_type

contains


  ! ---------------------------------------------------------------------------
  ! SUB: allocate all dynamic arrays
  ! ---------------------------------------------------------------------------
  subroutine alloc(s, nx, ny)

    class(state_type), intent(inout) :: s
    integer, intent(in) :: nx
    integer, intent(in) :: ny

    ! Allocate arrays
    allocate(s%x(nx))
    allocate(s%y(ny))
    allocate(s%topo(nx, ny))
    allocate(s%topo_dot_ice(nx, ny))
    allocate(s%temp_surf(nx, ny))
    allocate(s%temp_ice(nx, ny))
    allocate(s%temp_base(nx, ny))
    allocate(s%precip(nx, ny))
    allocate(s%runoff(nx, ny))
    allocate(s%ice_q_surf(nx, ny))
    allocate(s%ice_h(nx, ny))
    allocate(s%ice_h_dot(nx, ny))
    allocate(s%ice_h_soln(nx, ny))
    allocate(s%ice_ud(nx, ny))
    allocate(s%ice_vd(nx, ny))
    allocate(s%ice_us(nx, ny))
    allocate(s%ice_vs(nx, ny))

    ! Set initial value to zero (just to be sure)
    s%x = 0.0_rp
    s%y = 0.0_rp
    s%topo = 0.0_rp
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

  end subroutine alloc

end module state_mod 
