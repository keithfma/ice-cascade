! =============================================================================
! Hillslope erosion model, including variables and procedures
!
! Contains:
!   type hill_type (public)
!   subroutine init (private, type-bound procedure)
!   subroutine run (private, type-bound procedure)
!   NOTE: list is not yet complete
! ============================================================================

module hill_module

use types, only: dp
use grid_module, only: grid_type

implicit none
private
public hill_type

  ! --------------------------------------------------------------------------- 
  ! TYPE: all variables and procedures for the hillslope model component
  ! ---------------------------------------------------------------------------
  type hill_type
    logical                         :: on        ! enable/disable model
    type(grid_type), pointer        :: g         ! pointer to shared grid object
    real(dp), pointer               :: z(:,:)    ! pointer to shared topo
    real(dp), pointer               :: zref(:,:) ! pointer to shared topo solution
    real(dp)                        :: D         ! diffusivity, [m**2/a]
    real(dp)                        :: dtMax     ! max stable step CFL, [a]
    character(len=100)              :: nbcName   ! north BC name
    character(len=100)              :: sbcName   ! south BC name
    character(len=100)              :: wbcName   ! west BC name
    character(len=100)              :: ebcName   ! east BC name
    procedure (bc), pointer, nopass :: nbc       ! set north BC
    procedure (bc), pointer, nopass :: sbc       ! set south BC
    procedure (bc), pointer, nopass :: wbc       ! set west BC
    procedure (bc), pointer, nopass :: ebc       ! set east BC
    logical                         :: zSolnOn   ! enable/disable topo soln
    character(len=100)              :: zSolnName ! topo soln name
    procedure (soln), pointer, pass :: zSoln     ! topo soln
  contains
    procedure, pass                 :: init      ! set members
    procedure, pass                 :: run       ! run model                   
  end type hill_type


  ! ---------------------------------------------------------------------------
  ! FUNC TEMPLATE: common form for the bc functions
  ! ---------------------------------------------------------------------------
  abstract interface
    function bc(edge, intr) result(bnd)
      import               :: dp              ! use special types
      real(dp), intent(in) :: edge(:)         ! domain edge, elev [m]
      real(dp), intent(in) :: intr(:)         ! (not used for this bc)
      real(dp)             :: bnd(size(edge)) ! bc points, elev [m]
    end function bc
  end interface


  ! ---------------------------------------------------------------------------
  ! SUB TEMPLATE: common form for exact topography solutions
  ! ---------------------------------------------------------------------------
  abstract interface
    subroutine soln(h, t) 
      import                              :: dp, hill_type ! use special types
      class(hill_type), intent(in)        :: h             ! model object
      real(dp), intent(in)                :: t             ! model time
    end subroutine soln
  end interface


  ! ---------------------------------------------------------------------------
  ! PARAMETERS
  ! ---------------------------------------------------------------------------
  real(dp), parameter :: pi = 4.0_dp*atan(1.0_dp)
  real(dp), parameter :: hill_ss_t1 = 1.0_dp ! benchmark, hill, steady state
  real(dp), parameter :: hill_ss_tm = 9.0_dp ! benchmark, hill, steady state

contains

  ! --------------------------------------------------------------------------- 
  ! SUB: initialize a hillslope model object
  ! --------------------------------------------------------------------------- 
  subroutine init(h, g, z, zref)

    class(hill_type), intent(out)       :: h         ! object to initialize
    type(grid_type), intent(in), target :: g         ! grid, shared 
    real(dp), intent(inout), target     :: z(:,:)    ! topography, shared
    real(dp), intent(inout), target     :: zref(:,:) ! topography, shared

    real(dp) :: dx2, dy2

    ! associate pointers with shared objects
    h%g => g
    h%z => z
    h%zref => zref

    ! associate pointers with selected procedures
    call set_bc_proc(h%nbcName, h%nbc)
    call set_bc_proc(h%sbcName, h%sbc)
    call set_bc_proc(h%wbcName, h%wbc)
    call set_bc_proc(h%ebcName, h%ebc)
    call set_soln_proc(h%zSolnName, h%zSolnOn, h%zSoln)

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


  ! ---------------------------------------------------------------------------
  ! SUB: parse topo solution name and associate the soln procedure pointer
  ! ---------------------------------------------------------------------------
  subroutine set_soln_proc(str, on, ptr)

    character(len=*), intent(in)           :: str ! solution name
    logical                                :: on  ! enable/disable soln
    procedure (soln), pointer, intent(out) :: ptr ! procedure pointer to be set

    select case (str)

      case ("bench_hill_ss")
        print *, "Hill: exact solution for steady-state benchmark selected."
        on = .true.
        ptr => soln_bench_hill_ss

      case default 
        print *, "Hill: no exact solution for topography selected."
        on = .false.
        ptr => NULL()
   
      end select
    
  end subroutine set_soln_proc


  ! ---------------------------------------------------------------------------
  ! SUB: parse BC name and associate the BC procedure pointer
  ! ---------------------------------------------------------------------------
  subroutine set_bc_proc(str, ptr)

    character(len=*), intent(in)         :: str ! BC name
    procedure (bc), pointer, intent(out) :: ptr ! procedure pointer to be set

    select case (str)
      
      case ("zero_grad")
        ptr => bc_zero_grad

      case ("bench_hill_ss_sin")
        ptr => bc_bench_hill_ss_sin
     
      case ("bench_hill_ss_const")
        ptr => bc_bench_hill_ss_const
      
      case default 
        print *, "Invalid name for hillslope BC: ", trim(str)
        stop -1
   
      end select
    
  end subroutine set_bc_proc
  

  ! ---------------------------------------------------------------------------
  ! FUNC: Boundary condition, zero surface gradient normal to boundary (no-flux) 
  ! ---------------------------------------------------------------------------
  function bc_zero_grad(edge, intr) result(bnd)

    real(dp), intent(in) :: edge(:)         ! domain edge, elev [m]
    real(dp), intent(in) :: intr(:)         ! (not used for this BC)
    real(dp)             :: bnd(size(edge)) ! bc points, elev [m]

    bnd = edge ! surface gradient -> 1, then flux -> 0
    
    return
  end function bc_zero_grad
 

  ! ---------------------------------------------------------------------------
  ! FUNC: Boundary condition, Benchmark, Hill, Steady-state, sin-wave Dirichlet 
  ! ---------------------------------------------------------------------------
  function bc_bench_hill_ss_sin(edge, intr) result(bnd)

    real(dp), intent(in) :: edge(:)         ! domain edge, elev [m]
    real(dp), intent(in) :: intr(:)         ! (not used for this BC)
    real(dp)             :: bnd(size(edge)) ! bc points, elev [m]

    integer  :: n, i
    real(dp) :: c

    n = size(bnd)
    c = pi/(n-3)
    do i = 1,n
      bnd(i) = hill_ss_tm*sin(c*(i-2))+hill_ss_t1
    end do

    return
  end function bc_bench_hill_ss_sin

  
  ! ---------------------------------------------------------------------------
  ! FUNC: Boundary condition, Benchmark, Hill, Steady-state, constant Dirichlet
  ! ---------------------------------------------------------------------------
  function bc_bench_hill_ss_const(edge, intr) result(bnd)

    real(dp), intent(in) :: edge(:)         ! domain edge, elev [m]
    real(dp), intent(in) :: intr(:)         ! (not used for this BC)
    real(dp)             :: bnd(size(edge)) ! bc points, elev [m]

    bnd = hill_ss_t1 

    return
  end function bc_bench_hill_ss_const


  ! ---------------------------------------------------------------------------
  ! SUB: Exact solution, Benchmark, Hill, Steady-state
  ! ---------------------------------------------------------------------------
  subroutine soln_bench_hill_ss(h, t) 

    class(hill_type), intent(in) :: h ! model object
    real(dp), intent(in)         :: t ! model time

    ! DUMMY, REPLACE WITH ACTUAL SOLUTION
    h%zref = t

  end subroutine soln_bench_hill_ss

end module hill_module
