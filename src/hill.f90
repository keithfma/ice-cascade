! =============================================================================
! Hillslope erosion model, including variables and procedures
!
! Contains:
!   type hill_type (public)
!   subroutine init (private, type-bound procedure)
!   subroutine run (private, type-bound procedure)
! ============================================================================

module hill_module

use types, only: dp
use grid_module, only: grid_type

implicit none
private
public hill_type

  ! ---------------------------------------------------------------------------
  ! PARAMETERS
  ! ---------------------------------------------------------------------------
  real(dp), parameter :: pi = 4.0_dp*atan(1.0_dp)
  real(dp), parameter :: ssT1 = 0.0_dp ! benchmark: steady state
  real(dp), parameter :: ssTm = 1.0_dp ! benchmark: steady state


  ! ---------------------------------------------------------------------------
  ! INTERFACE: Template for the BC functions
  ! ---------------------------------------------------------------------------
  abstract interface
    function bc(edge, intr) result(bnd)
      import :: dp
      real(dp), intent(in) :: edge(:)         ! domain edge, elev [m]
      real(dp), intent(in) :: intr(:)         ! (not used for this BC)
      real(dp)             :: bnd(size(edge)) ! bc points, elev [m]
    end function bc
  end interface


  ! --------------------------------------------------------------------------- 
  ! TYPE: all variables and procedures for the hillslope model component
  ! ---------------------------------------------------------------------------
  type hill_type
    logical                                 :: on      ! enable/disable model
    type(grid_type), pointer                :: g       ! pointer to shared grid object
    real(dp), pointer                       :: z(:,:)  ! pointer to shared topography array
    real(dp)                                :: D       ! diffusivity, [m**2/a]
    real(dp)                                :: dtMax   ! maximum stable timestep from CFL, [a]
    character(len=10)                       :: nbcName ! north BC name
    character(len=10)                       :: sbcName ! south BC name
    character(len=10)                       :: wbcName ! west BC name
    character(len=10)                       :: ebcName ! east BC name
    procedure (bc), pointer, nopass         :: nbc     ! set north BC
    procedure (bc), pointer, nopass         :: sbc     ! set south BC
    procedure (bc), pointer, nopass         :: wbc     ! set west BC
    procedure (bc), pointer, nopass         :: ebc     ! set east BC
  contains
    procedure, pass                         :: init    ! set members
    procedure, pass                         :: run     ! run model                   
  end type hill_type

contains

  ! --------------------------------------------------------------------------- 
  ! SUB: initialize a hillslope model object
  ! --------------------------------------------------------------------------- 
  subroutine init(h, g, z)

    class(hill_type), intent(out)       :: h      ! object to initialize
    type(grid_type), intent(in), target :: g      ! grid, shared 
    real(dp), intent(inout), target     :: z(:,:) ! topography, shared

    real(dp) :: dx2, dy2

    ! associate pointers with shared objects
    h%g => g
    h%z => z

    ! associate pointers with selected procedures
    call choose_bc(h%nbcName, h%nbc)
    call choose_bc(h%sbcName, h%sbc)
    call choose_bc(h%wbcName, h%wbc)
    call choose_bc(h%ebcName, h%ebc)

    ! compute CFL timestep
    dx2 = g%dx**2.0_dp
    dy2 = g%dy**2.0_dp
    h%dtMax = 1.0_dp/(1.0_dp/dx2+1.0_dp/dy2)/(2.0_dp*h%D)

  end subroutine init

  
  ! ---------------------------------------------------------------------------
  ! SUB: parse BC name and associate the BC procedure pointer
  ! ---------------------------------------------------------------------------
  subroutine choose_bc(str, ptr)

    character(len=*), intent(in)                :: str ! BC name
    procedure (bc), pointer, intent(out) :: ptr ! procedure pointer to be set

    if ('noflux' .eq. trim(str))         ptr => bc_noflux
    if ('bench_ss_sin' .eq. trim(str))   ptr => bc_bench_ss_sin
    if ('bench_ss_const' .eq. trim(str)) ptr => bc_bench_ss_sin
    
  end subroutine choose_bc

  ! ---------------------------------------------------------------------------
  ! SUB: run the hillslope model for a specified duration
  ! ---------------------------------------------------------------------------
  subroutine run (H, duration)

    class(hill_type), intent(inout) :: H
    real(dp), intent(in) :: duration

    real(dp) :: time, dt
  
    time = 0.0_dp
    do while (time .lt. duration)
     
      ! select stable time step
      dt = min(duration-time, H%dtMax)

      ! apply boundary conditions
      H%z(:,H%G%nx+2) = H%nbc( H%z(:,H%G%nx+2), H%z(:,H%G%nx+1) )

      ! simulate diffusion

      ! increment time
      time = time+dt

    end do

  end subroutine

  ! ---------------------------------------------------------------------------
  ! FUNC: Boundary condition, no-flux (i.e. zero surface gradient) 
  ! ---------------------------------------------------------------------------
  function bc_noflux(edge, intr) result(bnd)

    real(dp), intent(in) :: edge(:)         ! domain edge, elev [m]
    real(dp), intent(in) :: intr(:)         ! (not used for this BC)
    real(dp)             :: bnd(size(edge)) ! bc points, elev [m]

    bnd = edge ! surface gradient -> 0, then flux -> 0
    
    return
  end function bc_noflux
  
  ! ===========================================================================
  ! Steady-state benchmark 
  ! ===========================================================================
  !  
  ! Steady state case with sinusoidal topography at the northern boundary, and
  ! constant topography elsewhere. Solution is derived at:
  ! http://www.mhhe.com/engcs/mech/holman/graphics/samplech_3.pdf

  ! ---------------------------------------------------------------------------
  ! FUNC: Steady-state benchmark, boundary condition, sin-wave Dirichlet 
  ! ---------------------------------------------------------------------------
  function bc_bench_ss_sin(edge, intr) result(bnd)

    real(dp), intent(in) :: edge(:)         ! domain edge, elev [m]
    real(dp), intent(in) :: intr(:)         ! (not used for this BC)
    real(dp)             :: bnd(size(edge)) ! bc points, elev [m]

    integer :: n, W, i

    ! interior points are at 0:W-1, boundary points are at -1 and W
    n = size(intr)
    W = n-3
    do i = 1, n
      bnd(i) = ssTm*sin(pi*(i-2)/W)+ssT1
    enddo

    return
  end function bc_bench_ss_sin

  ! ---------------------------------------------------------------------------
  ! FUNC: Steady-state benchmark, boundary condition, constant value
  ! ---------------------------------------------------------------------------
  function bc_bench_ss_const(edge, intr) result(bnd)

    real(dp), intent(in) :: edge(:)         ! domain edge, elev [m]
    real(dp), intent(in) :: intr(:)         ! (not used for this BC)
    real(dp)             :: bnd(size(edge)) ! bc points, elev [m]

    bnd = ssT1

    return
  end function bc_bench_ss_const

end module hill_module
