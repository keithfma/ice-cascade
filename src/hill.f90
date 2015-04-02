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
    logical                         :: on          ! enable/disable model
    logical                         :: write_soln  ! output flag
    logical                         :: write_dzdt  ! output flag
    integer                         :: nx          ! num grid points in x-dir, [1]
    integer                         :: ny          ! num grid points in y-dir, [1]
    real(dp)                        :: dx          ! grid spacing in x-dir, [m]
    real(dp)                        :: dy          ! grid spacing in y-dir, [m]
    real(dp)                        :: D           ! diffusivity, [m**2/a]
    real(dp)                        :: dtMax       ! max stable step CFL, [a]
    real(dp), allocatable           :: x(:)        ! x coordinate vector, [m]
    real(dp), allocatable           :: y(:)        ! y coordinate vector, [m]
    real(dp), allocatable           :: dzdt(:,:)   ! topo rate of change, [m/a]
    character(len=100)              :: nbcName     ! north BC name
    character(len=100)              :: sbcName     ! south BC name
    character(len=100)              :: wbcName     ! west BC name
    character(len=100)              :: ebcName     ! east BC name
    character(len=100)              :: solnName    ! topo soln name
    procedure (bc), pointer, nopass :: nbc         ! set north BC
    procedure (bc), pointer, nopass :: sbc         ! set south BC
    procedure (bc), pointer, nopass :: wbc         ! set west BC
    procedure (bc), pointer, nopass :: ebc         ! set east BC
    procedure (soln), pointer, pass :: solve       ! compute topo soln
  contains
    procedure, pass                 :: init        ! initialize all components
    procedure, pass                 :: run         ! run model                   
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
  ! FUNC TEMPLATE: common form for exact topography solutions
  ! ---------------------------------------------------------------------------
  abstract interface
    function soln(h, t) result(z)
      import                          :: dp, hill_type ! use special types
      class(hill_type), intent(inout) :: h             ! model object
      real(dp), intent(in)            :: t             ! model time
      real(dp), dimension(h%nx,h%ny)  :: z             ! soln for topo
    end function soln
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
  !   Note: components D, dtMax, *Name, and write_dzdt must be set before init()
  ! --------------------------------------------------------------------------- 
  subroutine init(h, g)

    class(hill_type), intent(inout) :: h ! object to initialize
    type(grid_type), intent(in)     :: g ! grid 
    
    if (h%on .eqv. .false.) then
      ! model disabled, clear all object components
      h%write_soln = .false.
      h%write_dzdt = .false.
      h%nx = -1
      h%ny = -1
      h%dx = -1.0_dp
      h%dy = -1.0_dp
      h%D = -1.0_dp
      h%dtMax = -1.0_dp
      if (allocated(h%x) .eqv. .true.) deallocate(h%x)
      if (allocated(h%y) .eqv. .true.) deallocate(h%y)
      if (allocated(h%dzdt) .eqv. .true.) deallocate(h%dzdt)
      allocate(h%x(1), h%y(1), h%dzdt(1,1))
      h%x = -1.0_dp
      h%y = -1.0_dp
      h%dzdt = -1.0_dp
      h%nbcName = "none"
      h%sbcName = "none"
      h%ebcName = "none"
      h%wbcName = "none"
      h%solnName = "none"
      h%nbc => NULL()
      h%sbc => NULL()
      h%wbc => NULL()
      h%ebc => NULL()
      h%solve => NULL()
    else
      ! model enabled, set all object components
      h%nx = g%nx
      h%ny = g%ny
      h%dx = g%dx
      h%dy = g%dy
      if (allocated(h%x) .eqv. .true.) deallocate(h%x)
      if (allocated(h%y) .eqv. .true.) deallocate(h%y)
      if (allocated(h%dzdt) .eqv. .true.) deallocate(h%dzdt)
      allocate(h%x(h%nx+2), h%y(h%ny+2), h%dzdt(h%nx+2, h%ny+2))
      h%x = g%x
      h%y = g%y
      call set_bc_proc(h%nbcName, h%nbc)
      call set_bc_proc(h%sbcName, h%sbc)
      call set_bc_proc(h%wbcName, h%wbc)
      call set_bc_proc(h%ebcName, h%ebc)
      call set_soln_proc(h%solnName, h%write_soln, h%solve)
      h%dtMax = 1.0_dp/(h%dx**-2.0_dp+h%dy**-2.0_dp)/(2.0_dp*h%D)
    end if

  end subroutine init

  ! ---------------------------------------------------------------------------
  ! SUB: run the hillslope model for a specified duration
  ! ---------------------------------------------------------------------------
  subroutine run (h, z, duration)

    class(hill_type), intent(inout) :: h        ! hill model def
    real(dp), intent(inout)         :: z(:,:)   ! topography
    real(dp), intent(in)            :: duration ! runtime
    
    integer :: north, south, east, west, i, j
    real(dp) :: time, dt, dx2inv, dy2inv, cpt, laplace
  
    ! define indices of edge points, for convenience
    north = h%ny+1
    south = 2
    east = h%nx+1
    west = 2

    time = 0.0_dp
    do while (time .lt. duration)
     
      ! select stable time step
      dt = min(duration-time, h%dtMax)

      ! apply boundary conditions
      z(:,north+1) = h%nbc( z(:,north), z(:,north-1) )
      z(:,south-1) = h%sbc( z(:,south), z(:,south+1) )
      z(east+1,:) = h%ebc( z(east,:), z(east-1,:) )
      z(west-1,:) = h%wbc( z(west,:), z(west+1,:) )

      ! compute diffusion, 5-point stencil
      dx2inv = h%dx**-2.0_dp 
      dy2inv = h%dy**-2.0_dp
      do j = south, north  
        do i = west, east
          cpt = -2.0_dp*z(i,j) 
          laplace = dx2inv*(z(i+1,j)+cpt+z(i-1,j)) + &
                    dy2inv*(z(i,j+1)+cpt+z(i,j-1))
          z(i,j) = z(i,j) + dt*h%D*laplace 
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
        on = .true.
        ptr => solve_bench_hill_ss

      case ("none")
        on = .false.
        ptr => NULL()

      case default 
        print *, "Invalid name for hillslope solution: ", trim(str)
        stop -1
   
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
  ! FUNC: Exact solution, Benchmark, Hill, Steady-state
  ! ---------------------------------------------------------------------------
  function solve_bench_hill_ss(h, t) result(z)

    class(hill_type), intent(inout) :: h ! model object
    real(dp), intent(in)            :: t ! model time
    real(dp), dimension(h%nx,h%ny)  :: z ! soln for topo

    ! DUMMY, REPLACE WITH ACTUAL SOLUTION
    z = t

  end function solve_bench_hill_ss

end module hill_module
