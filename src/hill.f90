! =============================================================================
! Hillslope erosion model, including variables and procedures
! ============================================================================
!
! Contains:
!   type hillslope (public)
!   subroutine init (private, type-bound procedure)
!   subroutine run (private, type-bound procedure)
!!

module ic_hill_module

use types, only: dp
use ic_grid_module, only: grid_type

implicit none
private
public hill_type

  ! Define constants
  real(dp), parameter :: pi = 4.0_dp*atan(1.0_dp)
  !! Steady-State benchmark constants
  real(dp), parameter :: T1 = 0.0_dp
  real(dp), parameter :: Tm = 1.0_dp

  ! --------------------------------------------------------------------------- 
  ! TYPE: all variables and procedures for the hillslope model component
  ! ---------------------------------------------------------------------------
  type hill_type
    ! variables
    logical                  :: on      ! enable/disable model component (used as failsafe)
    type(grid_type), pointer :: g       ! pointer to shared grid object
    real(dp), pointer        :: z(:,:)  ! pointer to shared topography array
    real(dp)                 :: D       ! diffusivity, [m**2/a]
    real(dp)                 :: dtMax   ! maximum stable timestep from CFL, [a]
    character(len=10)        :: nbcName ! string defining north BC
    procedure (bc), pointer  :: nbc     ! function that sets north BC
    character(len=10)        :: sbcName ! string defining north BC
    procedure (bc), pointer  :: sbc     ! sets south BC
    character(len=10)        :: ebcName ! string defining north BC
    procedure (bc), pointer  :: ebc     ! sets east BC
    character(len=10)        :: wbcName ! string defining north BC
    procedure (bc), pointer  :: wbc     ! sets west BC
  contains
    procedure init                      ! set all member variables
  end type hill_type

contains

  ! --------------------------------------------------------------------------- 
  ! SUB: initialize a hillslope model object
  ! --------------------------------------------------------------------------- 
  subroutine init(h, g, z)

    type(hill_type), intent(out)        :: h
    type(grid_type), intent(in), target :: g
    real(dp), intent(inout), target        :: z(:,:)

    real(dp) :: dx2, dy2

    ! associate pointers with shared objects
    h%g => g
    h%z => z

    ! compute CFL timestep
    dx2 = g%dx**2.0_dp
    dy2 = g%dy**2.0_dp
    h%dtMax = 1.0_dp/(1.0_dp/dx2+1.0_dp/dy2)/(2.0_dp*h%D)

  end subroutine init

  
  ! ---------------------------------------------------------------------------
  ! FUNC: Boundary condition, dummy, defines the generic form of a BC function
  ! ---------------------------------------------------------------------------
  function bc(edge, intr) result(bnd)
    real(dp), intent(in) :: edge(:)         ! domain edge, elev [m]
    real(dp), intent(in) :: intr(:)         ! 1-pt interior from edge, elev [m]
    real(dp)             :: bnd(size(edge)) ! bc points, elev [m]

    bnd = 0.0_dp
    print *, 'function bc in hill module is not meant to be run, but you ran it.'
    stop

    return
  end function bc




  ! ---------------------------------------------------------------------------
  ! FUNC: Boundary condition, no-flux (i.e. zero surface gradient) 
  ! ---------------------------------------------------------------------------
  function bc_noflux(edge, intr) result(bnd)
    real(dp), intent(in) :: edge(:)         ! domain edge, elev [m]
    real(dp), intent(in) :: intr(:)         ! (not used for this BC)
    real(dp)             :: bnd(size(edge)) ! bc points, elev [m]

    bnd = edge ! surface gradient -> 0, then flux -> 0

  end function bc_noflux


  ! ---------------------------------------------------------------------------
  ! SUB: run the hillslope model for a specified duration
  ! ---------------------------------------------------------------------------

  
  ! ===========================================================================
  ! Steady-state benchmark 
  ! ===========================================================================
  !  
  ! Steady state case with sinusoidal topography at the northern boundary, and
  ! constant topography elsewhere. Solution is derived at:
  ! http://www.mhhe.com/engcs/mech/holman/graphics/samplech_3.pdf

  ! ---------------------------------------------------------------------------
  ! func: Steady-state benchmark, set sin-wave Dirichley BC for 'north' edge
  ! ---------------------------------------------------------------------------
  function bench_ss_bc_sin(intr) result(bnd) 
  
    real(dp), intent(in) :: intr(:)         ! vector of adjacent interior points
    real(dp)             :: bnd(size(intr)) ! vector of boundary points

    integer :: n, W, i

    ! interior points are at 0:W-1, boundary points are at -1 and W
    n = size(intr)
    W = n-3
    do i = 1, n
      bnd(i) = Tm*sin(pi*(i-2)/W)+T1
    enddo

    return
  end function bench_ss_bc_sin

  ! ---------------------------------------------------------------------------
  ! func: Steady-state benchmark, set sin-wave Dirichley BC for 'north' edge
  ! ---------------------------------------------------------------------------
  function bench_ss_bc_const(intr) result(bnd)

    real(dp), intent(in) :: intr(:)         ! vector of adjacent interior points
    real(dp)             :: bnd(size(intr)) ! vector of boundary points

    bnd = T1

    return
  end function bench_ss_bc_const

end module ic_hill_module
