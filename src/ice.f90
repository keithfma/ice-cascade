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
    integer :: nx ! num grid pts in x-dir, [1]
    integer :: ny ! num grid pts in y-dir, [1]
    real(dp) :: dx ! grid spacing in x-dir, [m]
    real(dp) :: dy ! grid spacing in y-dir, [m]
    real(dp) :: c_b ! ice defm coeff, [Pa-3 a-1]
    real(dp), allocatable :: h(:,:) ! ice thickness, [m]
    real(dp), allocatable :: dhdt(:,:) ! ice thick. rate-of-change, [m a-1]
    real(dp), allocatable :: udefm(:,:) ! ice deformation velocity, x-dir [m a-1]
    real(dp), allocatable :: vdefm(:,:) ! ice deformation velocity, y-dir [m a-1]
    character(len=100) :: nbcName ! north BC name
    character(len=100) :: sbcName ! south BC name
    character(len=100) :: wbcName ! west BC name
    character(len=100) :: ebcName ! east BC name
    character(len=100) :: flowName ! ice flow method name
    procedure(bc_tmpl), pointer, nopass :: nbc ! set north BC
    procedure(bc_tmpl), pointer, nopass :: sbc ! set south BC
    procedure(bc_tmpl), pointer, nopass :: wbc ! set west BC
    procedure(bc_tmpl), pointer, nopass :: ebc ! set east BC
    procedure(flow_tmpl), pointer, pass :: flow ! ice flow  
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
  ! SUB TEMPLATE: common form for the bc functions
  ! ---------------------------------------------------------------------------
  abstract interface 
  subroutine flow_tmpl(i)
      import :: dp, ice_type ! use special types
    class(ice_type), intent(inout) :: i
    end subroutine flow_tmpl 
  end interface

contains

  ! --------------------------------------------------------------------------- 
  ! SUB: initialize an ice model object
  !   Note: components on, write*, c_b,  *Name,  must be set before init
  ! --------------------------------------------------------------------------- 
  subroutine init(i, g)

    class(ice_type), intent(inout) :: i ! ice obj to init
    type(grid_type), intent(in) :: g ! coord grid info

    ! model is enabled, set all members
    if (i%on) then
      i%nx = g%nx
      i%ny = g%ny
      i%dx = g%dx
      i%dy = g%dy
      if (allocated(i%h)) deallocate(i%h)
      allocate(i%h(i%nx+2, i%ny+2))
      if (allocated(i%dhdt)) deallocate(i%dhdt)
      allocate(i%dhdt(i%nx+2, i%ny+2))
      if (allocated(i%udefm)) deallocate(i%udefm)
      allocate(i%udefm(i%nx+2, i%ny+2))
      if (allocated(i%vdefm)) deallocate(i%vdefm)
      allocate(i%vdefm(i%nx+2, i%ny+2))
      call set_bc_proc(i%nbcName, i%nbc)
      call set_bc_proc(i%sbcName, i%sbc)
      call set_bc_proc(i%ebcName, i%ebc)
      call set_bc_proc(i%wbcName, i%wbc)
      call set_flow_proc(i%flowName, i%flow)

    ! model is disabled, unset all members
    else
      i%nx = -1
      i%ny = -1
      i%dx = -1.0_dp
      i%dy = -1.0_dp
      i%c_b = -1.0_dp
      if (allocated(i%h)) deallocate(i%h)
      allocate(i%h(1,1))
      i%h = -1.0_dp
      if (allocated(i%dhdt)) deallocate(i%dhdt)
      allocate(i%dhdt(1,1))
      i%dhdt = -1.0_dp
      if (allocated(i%udefm)) deallocate(i%udefm)
      allocate(i%udefm(1,1))
      i%udefm = -1.0_dp
      if (allocated(i%vdefm)) deallocate(i%vdefm)
      allocate(i%vdefm(1,1))
      i%vdefm = -1.0_dp
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


  ! ---------------------------------------------------------------------------
  ! SUB: parse flow method name and associate the flow procedure pointer
  ! ---------------------------------------------------------------------------
  subroutine set_flow_proc(str, ptr)

    character(len=*), intent(in)              :: str ! method name
    procedure (flow_tmpl), pointer, intent(out) :: ptr ! procedure pointer to be set

    select case (str)
      
      case ('none')
        ptr => NULL() ! NOTE: this will cause the program to fail by design

      case ('mahaffy')
        ptr => NULL()

      case default 
        print *, 'Invalid name for ice flow method: ', trim(str)
        stop -1
   
    end select
    
  end subroutine set_flow_proc

  
end module ice_module
