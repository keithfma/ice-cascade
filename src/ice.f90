! =============================================================================
! Glacier dynamics and erosion model, including variables and procedures
!
! Contains:
!   type ice_type (public)
!   NOTE: list is not yet complete
!
! References:
! (1) Bueler et al 2005...
!
! ============================================================================

module ice_module

use types, only: dp
use grid_module, only: grid_type

implicit none
private
public ice_type

  ! --------------------------------------------------------------------------- 
  ! TYPE: all variables and procedures for the glacier model component
  ! ---------------------------------------------------------------------------
  type ice_type
    logical :: on ! enable/disable model
    logical :: write_h ! output flag
    logical :: write_uvdefm ! output flag 
    logical :: write_soln ! output flag
    integer :: verbose ! output detail level
    integer :: nx ! num grid pts in x-dir, [1]
    integer :: ny ! num grid pts in y-dir, [1]
    real(dp) :: dx ! grid spacing in x-dir, [m]
    real(dp) :: dy ! grid spacing in y-dir, [m]
    real(dp) :: rhoi ! density of glacial ice, [kg/m3]
    real(dp) :: A0 ! ice flow law coeff prefactor, [Pa-3 a-1]
    real(dp), allocatable :: x(:) ! x coord [m]
    real(dp), allocatable :: y(:) ! y coord [m]
    real(dp), allocatable :: A(:,:) ! ice flow law coeff, [Pa-3 a-1]
    real(dp), allocatable :: h(:,:) ! ice thickness, [m]
    real(dp), allocatable :: dhdt(:,:) ! ice thick. rate-of-change, [m a-1]
    real(dp), allocatable :: udefm(:,:) ! ice deformation velocity, x-dir [m a-1]
    real(dp), allocatable :: vdefm(:,:) ! ice deformation velocity, y-dir [m a-1]
    real(dp), allocatable :: soln_h(:,:) ! exact ice thickness, [m]
    character(len=100) :: h0Name ! initial ice thickness name
    character(len=100) :: nbcName ! north BC name
    character(len=100) :: sbcName ! south BC name
    character(len=100) :: wbcName ! west BC name
    character(len=100) :: ebcName ! east BC name
    character(len=100) :: flowName ! ice flow method name
    character(len=100) :: solnName ! exact solution name
    procedure(bc_tmpl), pointer, nopass :: nbc_h ! set ice north BC
    procedure(bc_tmpl), pointer, nopass :: sbc_h ! set ice south BC
    procedure(bc_tmpl), pointer, nopass :: wbc_h ! set ice west BC
    procedure(bc_tmpl), pointer, nopass :: ebc_h ! set ice east BC
    procedure(bc_tmpl), pointer, nopass :: nbc_b ! set bed north BC
    procedure(bc_tmpl), pointer, nopass :: sbc_b ! set bed south BC
    procedure(bc_tmpl), pointer, nopass :: wbc_b ! set bed west BC
    procedure(bc_tmpl), pointer, nopass :: ebc_b ! set bed east BC
    procedure(flow_tmpl), pointer, pass :: flow  ! ice flow  
    procedure(soln_tmpl), pointer, pass :: solve ! exact solution  
  contains
    procedure, pass :: init ! initialize all components
    procedure, pass :: run ! run model                   
  end type ice_type


  ! ---------------------------------------------------------------------------
  ! FUNC TEMPLATE: common form for the bc functions
  ! ---------------------------------------------------------------------------
  abstract interface 
    function bc_tmpl(edge, intr, oppo) result(bnd)
      import :: dp                    ! use special types
      real(dp), intent(in) :: edge(:) ! domain edge 
      real(dp), intent(in) :: intr(:) ! domain edge-1
      real(dp), intent(in) :: oppo(:) ! opposite domain edge
      real(dp) :: bnd(size(edge))     ! bc points
    end function bc_tmpl 
  end interface

  ! ---------------------------------------------------------------------------
  ! SUB TEMPLATE: common form for the ice flow subroutines
  ! ---------------------------------------------------------------------------
  abstract interface 
    subroutine flow_tmpl(i)
      import :: dp, ice_type ! use special types
      class(ice_type), intent(inout) :: i
    end subroutine flow_tmpl 
  end interface

  ! ---------------------------------------------------------------------------
  ! SUB TEMPLATE: common form for the exact solution subroutines
  ! ---------------------------------------------------------------------------
  abstract interface 
    subroutine soln_tmpl(i, time)
      import                         :: dp, ice_type ! use special types
      class(ice_type), intent(inout) :: i
      real(dp), intent(in)           :: time
    end subroutine soln_tmpl 
  end interface

  ! ---------------------------------------------------------------------------
  ! PARAMETERS
  ! ---------------------------------------------------------------------------
  real(dp), parameter :: grav = 9.8067_dp
  real(dp), parameter :: bench_bueler_isothermal_a_m0 = 0.3_dp ! m/a

contains

  ! --------------------------------------------------------------------------- 
  ! SUB: initialize an ice model object
  !   Note: some components must be set before init
  ! --------------------------------------------------------------------------- 
  subroutine init(i, g)

    class(ice_type), intent(inout) :: i ! ice obj to init
    type(grid_type), intent(in) :: g ! coord grid info

    ! object is already initialized, exit with error
    if (allocated(i%x)) then
      print *, 'Attempted to initialize ice_type object twice, exiting.'
      stop -1
    end if

    ! model is enabled, init all members
    if (i%on) then
      allocate(i%x(g%nx));                
      allocate(i%y(g%ny));                
      allocate(i%A(g%nx+2, g%ny+2));      
      allocate(i%h(g%nx+2, g%ny+2));      
      allocate(i%dhdt(g%nx+2, g%ny+2));   
      allocate(i%udefm(g%nx+2, g%ny+2));  
      allocate(i%vdefm(g%nx+2, g%ny+2));  
      allocate(i%soln_h(g%nx+4, g%ny+2)); 
      i%nx = g%nx
      i%ny = g%ny
      i%dx = g%dx
      i%dy = g%dy
      i%x = g%x
      i%y = g%y
      i%A = i%A0
      i%h = 0.0_dp
      i%dhdt = 0.0_dp
      i%udefm = 0.0_dp
      i%vdefm = 0.0_dp
      i%soln_h = 0.0_dp

      ! parse bc names, set procedures
      call set_bc_proc(i%nbcName, i%nbc_h, i%nbc_b)
      call set_bc_proc(i%sbcName, i%sbc_h, i%sbc_b)
      call set_bc_proc(i%ebcName, i%ebc_h, i%ebc_b)
      call set_bc_proc(i%wbcName, i%wbc_h, i%wbc_b)

      ! parse ice flow model name, set procedure
      select case (i%flowName)
        case ('none')
          i%flow => NULL() ! NOTE: this will cause the program to fail by design
        case ('mahaffy')
          i%flow => NULL()! PLACEHOLDER, TO BE UPDATED
        case default 
          print *, 'Invalid name for ice flow method: ', trim(i%flowName)
          stop -1
      end select

      ! parse solution name, set procedures
      select case (i%solnName)
        case ('none')
          i%solve => NULL()
          i%write_soln = .false.
        case ('bueler_isothermal_a')
          i%solve => soln_bueler_isothermal_a
          i%write_soln = .true.
        case default 
          print *, 'Invalid name for ice flow method: ', trim(i%solnName)
          stop -1
      end select

      ! read initial ice thickness
      select case (i%h0Name)
        case ('zero')
          i%h = 0.0_dp
        case ('exact')
          print *, 'Invalid name for initial ice thickness: ', trim(i%h0Name)
          print *, '(Note: setting from exact solution is planned, but not yet implemented.)'
        case default 
          print *, 'Invalid name for initial ice thickness: ', trim(i%h0Name)
          print *, '(Note: reading from file is planned, but not yet implemented.)'
          stop -1
      end select

    ! model is disabled, unset all members
    else
      allocate(i%x(1))
      allocate(i%y(1))
      allocate(i%A(1,1))
      allocate(i%h(1,1))
      allocate(i%dhdt(1,1))
      allocate(i%udefm(1,1))
      allocate(i%vdefm(1,1))
      allocate(i%soln_h(1,1))
      i%write_h = .false.
      i%write_uvdefm = .false.
      i%write_soln = .false.
      i%nx = -1
      i%ny = -1
      i%dx = -1.0_dp
      i%dy = -1.0_dp
      i%A0 = -1.0_dp
      i%x = -1.0_dp
      i%y = -1.0_dp
      i%A = -1.0_dp
      i%h = -1.0_dp
      i%dhdt = -1.0_dp
      i%udefm = -1.0_dp
      i%vdefm = -1.0_dp
      i%soln_h = -1.0_dp
      i%h0Name = 'none'
      i%nbcName = 'none'
      i%sbcName = 'none'
      i%ebcName = 'none'
      i%wbcName = 'none'
      i%flowName = 'none'
      i%nbc_h => NULL()
      i%sbc_h => NULL()
      i%ebc_h => NULL()
      i%wbc_h => NULL()
      i%nbc_b => NULL()
      i%sbc_b => NULL()
      i%ebc_b => NULL()
      i%wbc_b => NULL()
      i%flow => NULL()
      i%solve => NULL()
    end if

  end subroutine init


  ! ---------------------------------------------------------------------------
  ! SUB: run the ice  model for a specified duration
  ! ---------------------------------------------------------------------------
  subroutine run(i)
    class(ice_type) :: i
  end subroutine run


  ! ---------------------------------------------------------------------------
  ! SUB: parse BC name and associate the BC procedure pointer
  ! ---------------------------------------------------------------------------
  subroutine set_bc_proc(str, ptr_h, ptr_b)

    character(len=*), intent(in) :: str                ! BC name
    procedure (bc_tmpl), pointer, intent(out) :: ptr_h ! proc ptr for ice
    procedure (bc_tmpl), pointer, intent(out) :: ptr_b ! proc ptr for bed

    select case (str)
      case ('none')
        ptr_h => NULL() ! NOTE: this will cause the program to fail by design
        ptr_b => NULL()
      case ('no_ice')
        ptr_h => bc_h_bnd_eq_zero
        ptr_b => bc_b_bnd_eq_adjacent
      case default 
        print *, 'Invalid name for ice BC: ', trim(str)
        stop -1
    end select
    
  end subroutine set_bc_proc


  ! ---------------------------------------------------------------------------
  ! FUNC: Boundary condition, ice thickness, set to zero
  ! ---------------------------------------------------------------------------
  function bc_h_bnd_eq_zero(edge, intr, oppo) result(bnd)

    real(dp), intent(in) :: edge(:) ! (not used)
    real(dp), intent(in) :: intr(:) ! (not used)
    real(dp), intent(in) :: oppo(:) ! (not used)
    real(dp) :: bnd(size(edge))     ! bc points

    bnd = 0.0_dp

  end function bc_h_bnd_eq_zero 
  
  
  ! ---------------------------------------------------------------------------
  ! FUNC: Boundary condition, bed topography, bnd same as domain edge 
  ! ---------------------------------------------------------------------------
  function bc_b_bnd_eq_adjacent(edge, intr, oppo) result(bnd)

  real(dp), intent(in) :: edge(:)   ! domain edge
    real(dp), intent(in) :: intr(:) ! (not used)
    real(dp), intent(in) :: oppo(:) ! (not used)
    real(dp) :: bnd(size(edge))     ! bc points

    bnd = edge

  end function bc_b_bnd_eq_adjacent 


  ! ---------------------------------------------------------------------------
  ! SUB: Exact solution, Bueler et al 2005, isothermal, test A, NE quadrant 
  !   Solution comes from reference (1), equation 17. 
  ! ---------------------------------------------------------------------------
  subroutine soln_bueler_isothermal_a(i, time)

    class(ice_type), intent(inout) :: i
    real(dp), intent(in)           :: time

    integer :: p, q
    real(dp) :: M0, L, gam, c1, c2, e1, e2, r

    ! print output 
    if (i%verbose .eq. 2) then
      print *, 'Exact solution: Bueler isothermal A'
    end if

    ! constants
    M0 = bench_bueler_isothermal_a_m0
    L = maxval(i%x)-i%dx 
    gam = 2.0_dp/5.0_dp*i%A0*(i%rhoi*grav)**3 
    c1 = (4.0_dp*M0/gam)**(1.0_dp/8.0_dp)
    c2 = L**(4.0_dp/3.0_dp)
    e1 = 4.0_dp/3.0_dp
    e2 = 3.0_dp/8.0_dp 

    ! ice thickness
    do q = 1, i%ny+2
      do p = 1, i%nx+2
        r = sqrt(i%x(p)**2+i%y(q)**2)
        if (r .le. L) then
          i%soln_h(p,q) = c1*(c2-r**e1)**e2
        else
          i%soln_h(p,q) = 0.0_dp
        end if
      end do
    end do



 
  end subroutine soln_bueler_isothermal_a
  
end module ice_module
