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
use bc_module, only: bc, set_bc_proc

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
  ! TYPE: all variables and procedures for the hillslope model component
  ! ---------------------------------------------------------------------------
  type hill_type
    logical                                 :: on      ! enable/disable model
    type(grid_type), pointer                :: g       ! pointer to shared grid object
    real(dp), pointer                       :: z(:,:)  ! pointer to shared topography array
    real(dp)                                :: D       ! diffusivity, [m**2/a]
    real(dp)                                :: dtMax   ! maximum stable timestep from CFL, [a]
    character(len=100)                      :: nbcName ! north BC name
    character(len=100)                      :: sbcName ! south BC name
    character(len=100)                      :: wbcName ! west BC name
    character(len=100)                      :: ebcName ! east BC name
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
    call set_bc_proc(h%nbcName, h%nbc)
    call set_bc_proc(h%sbcName, h%sbc)
    call set_bc_proc(h%wbcName, h%wbc)
    call set_bc_proc(h%ebcName, h%ebc)

    ! compute CFL timestep
    dx2 = g%dx**2.0_dp
    dy2 = g%dy**2.0_dp
    h%dtMax = 1.0_dp/(1.0_dp/dx2+1.0_dp/dy2)/(2.0_dp*h%D)

  end subroutine init

  ! ---------------------------------------------------------------------------
  ! SUB: run the hillslope model for a specified duration
  ! ---------------------------------------------------------------------------
  subroutine run (H, duration)

    class(hill_type), intent(inout) :: H
    real(dp), intent(in) :: duration
    
    integer :: north, south, east, west, i, j
    real(dp) :: time, dt, dx2inv, dy2inv, cpt, laplace
  
    ! define indices of edge points, for convenience
    north = H%G%ny+1
    south = 2
    east = H%G%nx+1
    west = 2

    time = 0.0_dp
    do while (time .lt. duration)
     
      ! select stable time step
      dt = min(duration-time, H%dtMax)

      ! apply boundary conditions
      H%z(:,north+1) = H%nbc( H%z(:,north), H%z(:,north-1) )
      H%z(:,south-1) = H%sbc( H%z(:,south), H%z(:,south+1) )
      H%z(east+1,:) = H%ebc( H%z(east,:), H%z(east-1,:) )
      H%z(west-1,:) = H%wbc( H%z(west,:), H%z(west+1,:) )

      ! compute diffusion, 5-point stencil
      dx2inv = H%G%dx**-2.0_dp 
      dy2inv = H%G%dy**-2.0_dp
      do j = south, north  
        do i = west, east
          cpt = -2.0_dp*H%z(i,j) 
          laplace = dx2inv*(H%z(i+1,j)+cpt+H%z(i-1,j)) + &
                    dy2inv*(H%z(i,j+1)+cpt+H%z(i,j-1))
          H%z(i,j) = H%z(i,j) + dt*H%D*laplace 
        end do
      end do

      ! increment time
      time = time+dt

    end do

  end subroutine
  


end module hill_module
