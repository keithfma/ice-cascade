! Coordinate grid variables and related methods.
!
! Contains:
!   type grid
!   subroutine interpolate
!!

module ic_grid_module

  use types, only: dp

  implicit none
  private
  public grid

  ! TYPE: Coordinate grid
  type grid

    integer :: nx, ny               ! num grid points in x- and y-dir, [1]
    real(dp) :: dx, dy              ! grid spacing in x- and y-dir, [m]
    real, allocatable :: x(:), y(:) ! x- and y- coordinate vectors, [m]

  contains

    procedure init

  end type grid

contains

  ! SUBROUTINE: Initialize grid from dimensions and spacing, adding a 1-point
  ! border for numerical boundary conditions
  subroutine init(g, nx, ny, dx, dy)

    ! arguments
    type(grid), intent(out) :: g  ! coordinate grid to be initialized
    integer, intent(in) :: nx, ny ! num grid points in x- and y-dir, [1]
    real(dp) :: dx, dy            ! x- and y- coordinate vectors, [m]
    ! internal vars
    integer :: i 

    g%nx = nx+2 ! add 1-pt boundary
    g%ny = ny+2 ! add 1-pt boundary
    g%dx = dx
    g%dy = dy

    ! allocate
    if (allocated(g%x) .eqv. .true.) deallocate(g%x)
    if (allocated(g%y) .eqv. .true.) deallocate(g%y)
    allocate(g%x(g%nx), g%y(g%nx))

    ! populate
    do i = 1, g%nx
      g%x(i) = real(i-2, dp)*g%dx
    enddo
    do i = 1, g%nx
      g%x(i) = real(i-2, dp)*g%dx
    enddo

  end subroutine init

end module ic_grid_module
