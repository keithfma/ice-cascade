! =============================================================================
! Common variables for ice-cascade model. 
!
! Contains:
!   type common_type (public)
!
! ============================================================================

module common_mod

use kinds_mod, only: rp

implicit none
private
public :: common_type

  ! --------------------------------------------------------------------------- 
  ! TYPE: model state variables
  ! ---------------------------------------------------------------------------
  type common_type
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
    real(rp), allocatable :: temp_surf(:,:) ! temp at ice/bedrock surface, [C]
    real(rp), allocatable :: temp_ice(:,:) ! mean ice temperature, [C]
    real(rp), allocatable :: temp_base(:,:) ! temp at bedrock surface, [C]
    real(rp), allocatable :: precip(:,:) ! precipitation rate, [m_water/a]
    real(rp), allocatable :: runoff(:,:) ! runoff rate, [m_water/a]
    real(rp), allocatable :: ice_q_surf(:,:) ! surface ice flux, [m_ice/a]
    real(rp), allocatable :: ice_h(:,:) ! ice thickness, [m]
    real(rp), allocatable :: ice_h_dot(:,:) ! ice thickness rate-of-change, [m/a]
  contains
    procedure, pass :: init ! initialize object
  end type common_type

contains


  ! ---------------------------------------------------------------------------
  ! SUB: initialize object
  ! ---------------------------------------------------------------------------
  subroutine init(c)

    class(common_type), intent(inout) :: c

    integer :: i

    ! Allocate arrays
    allocate(c%x(c%nx+2))
    allocate(c%y(c%ny+2))
    allocate(c%topo(c%nx+2, c%ny+2))
    allocate(c%temp_surf(c%nx+2, c%ny+2))
    allocate(c%temp_ice(c%nx+2, c%ny+2))
    allocate(c%temp_base(c%nx+2, c%ny+2))
    allocate(c%precip(c%nx+2, c%ny+2))
    allocate(c%runoff(c%nx+2, c%ny+2))
    allocate(c%ice_q_surf(c%nx+2, c%ny+2))
    allocate(c%ice_h(c%nx+2, c%ny+2))
    allocate(c%ice_h_dot(c%nx+2, c%ny+2))

    ! Populate arrays
    do i = 1, c%nx+2
      c%x(i) = real(i-2, rp)*c%dx
    enddo
    do i = 1, c%ny+2
      c%y(i) = real(i-2, rp)*c%dy
    enddo

  end subroutine init

end module common_mod
