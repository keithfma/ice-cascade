! Topography and coordinate grid variables and related methods.
!
! Contains:
!   type topo_type (public)
!   subroutine init (private, type-bound procedure)
!   subroutine interpLowToHigh (PLANNED, public)
!   subroutine interpHighToLow (PLANNED, public)
!!

module ic_topo_module

  use types, only: dp

  implicit none
  private
  public topo_type

  ! TYPE: Coordinate grid
  type topo_type
    integer :: nx, ny               ! num grid points in x- and y-dir, [1]
    real(dp) :: dx, dy              ! grid spacing in x- and y-dir, [m]
    real, allocatable :: x(:), y(:) ! x- and y- coordinate vectors, [m]
    real, allocatable :: z(:,:)     ! surface elevation, [m]
  contains
    procedure init
  end type topo_type

contains

  ! SUBROUTINE: Initialize grid from dimensions and spacing, adding a 1-point
  ! border for numerical boundary conditions
  subroutine init(t, nx, ny, dx, dy, z)
    
    type(topo_type), intent(out) :: t      ! object to initialize
    integer, intent(in)          :: nx, ny ! num grid points in x- and y-dir, [1]
    real(dp), intent(in)         :: dx, dy ! x- and y- coordinate vectors, [m]
    real(dp), intent(in)         :: z(:,:) ! input file with elevation data

    integer :: i 

    t%nx = nx+2 ! add 1-pt boundary
    t%ny = ny+2 ! add 1-pt boundary
    t%dx = dx
    t%dy = dy

    ! allocate
    if (allocated(t%x) .eqv. .true.) deallocate(t%x)
    if (allocated(t%y) .eqv. .true.) deallocate(t%y)
    if (allocated(t%z) .eqv. .true.) deallocate(t%z)
    allocate(t%x(t%nx), t%y(t%nx), t%z(t%ny, t%nx))

    ! populate coordinate vectors
    do i = 1, t%nx
      t%x(i) = real(i-2, dp)*t%dx
    enddo
    do i = 1, t%nx
      t%x(i) = real(i-2, dp)*t%dx
    enddo

    ! populate topography
    t%z = z

  end subroutine init

end module ic_topo_module
