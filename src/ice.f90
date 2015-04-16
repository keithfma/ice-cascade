! =============================================================================
! Glacier dynamics and erosion model, including variables and procedures
!
! Contains:
!   type ice_type (public)
!   NOTE: list is not yet complete
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
    integer :: nx ! num grid pts in x-dir, [1]
    integer :: ny ! num grid pts in y-dir, [1]
    real(dp) :: dx ! grid spacing in x-dir, [m]
    real(dp) :: dy ! grid spacing in y-dir, [m]
    real(dp) :: c_b ! ice defm coeff, [Pa-3 a-1]
    real(dp), allocatable :: x(:) ! x coord [m]
    real(dp), allocatable :: y(:) ! y coord [m]
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
    procedure(bc_tmpl), pointer, nopass :: nbc ! set north BC
    procedure(bc_tmpl), pointer, nopass :: sbc ! set south BC
    procedure(bc_tmpl), pointer, nopass :: wbc ! set west BC
    procedure(bc_tmpl), pointer, nopass :: ebc ! set east BC
    procedure(flow_tmpl), pointer, pass :: flow ! ice flow  
    procedure(soln_tmpl), pointer, pass :: soln ! exact solution  
  contains
    procedure, pass :: init ! initialize all components
    procedure, pass :: run ! run model                   
  end type ice_type


  ! ---------------------------------------------------------------------------
  ! FUNC TEMPLATE: common form for the bc functions
  ! ---------------------------------------------------------------------------
  abstract interface 
    function bc_tmpl()
      import :: dp ! use special types
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

contains

  ! --------------------------------------------------------------------------- 
  ! SUB: initialize an ice model object
  !   Note: components on, write*, c_b,  *Name,  must be set before init
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
      allocate(i%h(g%nx+2, g%ny+2));      
      allocate(i%dhdt(g%nx+2, g%ny+2));   
      allocate(i%udefm(g%nx+2, g%ny+2));  
      allocate(i%vdefm(g%nx+2, g%ny+2));  
      allocate(i%soln_h(g%nx+2, g%ny+2)); 
      i%nx = g%nx
      i%ny = g%ny
      i%dx = g%dx
      i%dy = g%dy
      i%x = g%x
      i%y = g%y
      i%h = 0.0_dp
      i%dhdt = 0.0_dp
      i%udefm = 0.0_dp
      i%vdefm = 0.0_dp
      i%soln_h = 0.0_dp

      ! parse bc names, set procedures
      call set_bc_proc(i%nbcName, i%nbc)
      call set_bc_proc(i%sbcName, i%sbc)
      call set_bc_proc(i%ebcName, i%ebc)
      call set_bc_proc(i%wbcName, i%wbc)

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
          i%soln => NULL()
          i%write_soln = .false.
        case ('bueler_isothermal_a')
          i%soln => NULL() ! PLACEHOLDER, TO BE UPDATED
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
      i%c_b = -1.0_dp
      i%x = -1.0_dp
      i%y = -1.0_dp
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
      i%nbc => NULL()
      i%sbc => NULL()
      i%ebc => NULL()
      i%wbc => NULL()
      i%flow => NULL()
    end if

  end subroutine init


  ! ---------------------------------------------------------------------------
  ! SUB: run the ice  model for a specified duration
  ! ---------------------------------------------------------------------------
  subroutine run(i)
    class(ice_type) :: i
  end subroutine run


  ! ---------------------------------------------------------------------------
  ! SUB: init 1D real array
  ! ---------------------------------------------------------------------------
  subroutine init_var1(var, n)

    real(dp), allocatable, intent(inout) :: var(:) ! var to init
    integer, optional, intent(in)        :: n      ! allocated size

    if (allocated(var)) deallocate(var)
    allocate(var(n))
    
  end subroutine init_var1


  ! ---------------------------------------------------------------------------
  ! SUB: clear 1D real array
  ! ---------------------------------------------------------------------------
  subroutine clear_var1(var)

    real(dp), allocatable, intent(inout) :: var(:) ! var to clear

    if (allocated(var)) deallocate(var)
    allocate(var(1))
    var = -1.0_dp
    
  end subroutine clear_var1


  ! ---------------------------------------------------------------------------
  ! SUB: parse BC name and associate the BC procedure pointer
  ! ---------------------------------------------------------------------------
  subroutine set_bc_proc(str, ptr)

    character(len=*), intent(in)              :: str ! BC name
    procedure (bc_tmpl), pointer, intent(out) :: ptr ! procedure pointer to be set

    select case (str)
      case ('none')
        ptr => NULL() ! NOTE: this will cause the program to fail by design
      case ('no_ice')
        ptr => NULL()
      case default 
        print *, 'Invalid name for ice BC: ', trim(str)
        stop -1
    end select
    
  end subroutine set_bc_proc


!  ! ---------------------------------------------------------------------------
!  ! SUB TEMPLATE: common form for the exact solution subroutines
!  ! ---------------------------------------------------------------------------
!  abstract interface 
!    subroutine soln_tmpl(i, time)
!      import                         :: dp, ice_type ! use special types
!      class(ice_type), intent(inout) :: i
!      real(dp), intent(in)           :: time
!    end subroutine soln_tmpl 
!  end interface

  
end module ice_module
