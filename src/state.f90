! =============================================================================
! Shared parameters and state variables for ice-cascade. 
!
! Contains:
!   type state_type (public)
!
! ============================================================================

module state_mod

use kinds_mod, only: rp
use param_mod, only: param_type

implicit none
private
public :: state_type

  ! --------------------------------------------------------------------------- 
  ! TYPE: shared state variables
  ! ---------------------------------------------------------------------------
  type state_type
    real(rp) :: time_now ! current model time, [a]
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
    procedure, pass :: init ! initialize object
  end type state_type

contains


  ! ---------------------------------------------------------------------------
  ! SUB: initialize object
  ! ---------------------------------------------------------------------------
  subroutine init(s, p)

    class(state_type), intent(inout) :: s
    type(param_type), intent(in) :: p

    integer :: nxp, nyp

    ! add ghost points to array dimensions
    nxp = p%nx+2
    nyp = p%ny+2

    ! allocate arrays
    allocate(s%x(nxp))
    allocate(s%y(nyp))
    allocate(s%topo(nxp, nyp))
    allocate(s%topo_dot_ice(nxp, nyp))
    allocate(s%temp_surf(nxp, nyp))
    allocate(s%temp_ice(nxp, nyp))
    allocate(s%temp_base(nxp, nyp))
    allocate(s%precip(nxp, nyp))
    allocate(s%runoff(nxp, nyp))
    allocate(s%ice_q_surf(nxp, nyp))
    allocate(s%ice_h(nxp, nyp))
    allocate(s%ice_h_dot(nxp, nyp))
    allocate(s%ice_h_soln(nxp, nyp))
    allocate(s%ice_ud(nxp, nyp))
    allocate(s%ice_vd(nxp, nyp))
    allocate(s%ice_us(nxp, nyp))
    allocate(s%ice_vs(nxp, nyp))

    ! set initial value to zero (compiler default,  done explicitly  just to be sure)
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

  end subroutine init

end module state_mod 
