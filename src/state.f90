! =============================================================================
! Shared model state variables for ice-cascade. 
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
  ! TYPE: model state variables
  ! ---------------------------------------------------------------------------
  type state_type
    integer :: nx ! num grid points in x-dir, [1]
    integer :: ny ! num grid points in y-dir, [1]
    real(rp) :: dx ! grid spacing in x-dir, [m]
    real(rp) :: dy ! grid spacing in y-dir, [m]
    real(rp) :: rhoi ! density of glacial ice, [kg/m3]
    real(rp) :: time_now ! current time [a]
    real(rp) :: time_start ! start time [a]
    real(rp) :: time_finish ! finish time [a]
    real(rp) :: time_step ! time step [a]
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
  subroutine init(s)

    class(state_type), intent(inout) :: s

    integer :: i

    ! Allocate arrays
    allocate(s%x(s%nx+2))
    allocate(s%y(s%ny+2))
    allocate(s%topo(s%nx+2, s%ny+2))
    allocate(s%topo_dot_ice(s%nx+2, s%ny+2))
    allocate(s%temp_surf(s%nx+2, s%ny+2))
    allocate(s%temp_ice(s%nx+2, s%ny+2))
    allocate(s%temp_base(s%nx+2, s%ny+2))
    allocate(s%precip(s%nx+2, s%ny+2))
    allocate(s%runoff(s%nx+2, s%ny+2))
    allocate(s%ice_q_surf(s%nx+2, s%ny+2))
    allocate(s%ice_h(s%nx+2, s%ny+2))
    allocate(s%ice_h_dot(s%nx+2, s%ny+2))
    allocate(s%ice_h_soln(s%nx+2, s%ny+2))
    allocate(s%ice_ud(s%nx+2, s%ny+2))
    allocate(s%ice_vd(s%nx+2, s%ny+2))
    allocate(s%ice_us(s%nx+2, s%ny+2))
    allocate(s%ice_vs(s%nx+2, s%ny+2))

    ! Populate arrays
    do i = 1, s%nx+2
      s%x(i) = real(i-2, rp)*s%dx
    enddo
    do i = 1, s%ny+2
      s%y(i) = real(i-2, rp)*s%dy
    enddo
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
