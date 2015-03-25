! =============================================================================
! Coordinate grid variables and related methods.
!
! Contains:
!   type grid_type (public)
!   subroutine init (private, type-bound procedure)
!   subroutine smoothVar (PLANNED, public)
!   subroutine interpUp (PLANNED, public)
!   subroutine interpDown (PLANNED, public)
! =============================================================================

module grid_module

  use types, only: dp

  implicit none
  private
  public grid_type

  ! ---------------------------------------------------------------------------
  ! TYPE: Coordinate grid
  ! ---------------------------------------------------------------------------
  type :: grid_type
    integer           :: nx, ny     ! num grid points in x- and y-dir, [1]
    real(dp)          :: dx, dy     ! grid spacing in x- and y-dir, [m]
    real, allocatable :: x(:), y(:) ! x- and y- coordinate vectors, [m]
  contains
    procedure         :: init       ! initialization function
  end type grid_type

contains

  ! ---------------------------------------------------------------------------
  ! SUBROUTINE: Initialize grid from dimensions and spacing
  ! ---------------------------------------------------------------------------
  subroutine init(g)
    
    class(grid_type), intent(inout) :: g ! object to initialize
    integer :: i 

    ! allocate
    if (allocated(g%x) .eqv. .true.) deallocate(g%x)
    if (allocated(g%y) .eqv. .true.) deallocate(g%y)
    allocate(g%x(g%nx+2), g%y(g%ny+2))

    ! populate coordinate vectors
    do i = 1, g%nx+2
      g%x(i) = real(i-2, dp)*g%dx
    enddo
    do i = 1, g%ny+2
      g%y(i) = real(i-2, dp)*g%dy
    enddo

  end subroutine init

! subroutine smoothVar()
! end subroutine smoothVar

! subroutine interpUp()
! end subroutine interpUp

!  subroutine interpDown()
!  end subroutine interpDown

end module grid_module
