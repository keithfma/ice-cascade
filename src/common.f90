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
    real(rp), allocatable :: x(:) ! x coordinate vector, [m]
    real(rp), allocatable :: y(:) ! y coordinate vector, [m]
    real(rp), allocatable :: T(:,:) ! bedrock elevation, [m above sea level]
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

    ! Check for sane inputs
    
    ! positive grid dimensions 
    if ((c%nx .le. 0) .or. (c%ny .le. 0)) then
      print *, 'Invalid grid description: grid dimensions must be positive integers.'
      stop -1
    end if

    ! positive grid spacing
    if ((c%dx .le. 0.0_rp) .or. (c%dy .le. 0.0_rp)) then
      print *, 'Invalid grid description: grid spacings must be positive.'
      stop -1
    end if

    ! positive densities
    if (c%rhoi .le. 0.0_rp) then
      print *, 'Invalid physical constant: density must be positive.'
      stop -1
    end if


    ! Init variables

    ! coordinate vectors
    allocate(c%x(c%nx+2))
    allocate(c%y(c%ny+2))
    do i = 1, c%nx+2
      c%x(i) = real(i-2, rp)*c%dx
    enddo
    do i = 1, c%ny+2
      c%y(i) = real(i-2, rp)*c%dy
    enddo

    ! variable arrays
    allocate(c%T(c%nx+2, c%ny+2))

  end subroutine init

end module common_mod
