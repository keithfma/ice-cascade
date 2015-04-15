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
    logical :: write_h ! output flag, ice thickness
    logical :: write_vd ! output flag, ice defm veolcity
    integer :: nx ! num grid pts in x-dir, [1]
    integer :: ny ! num grid pts in y-dir, [1]
    real(dp) :: c_b ! ice defm coeff, [Pa-3 a-1]
    character(len=100) :: nbcName ! north BC name
    character(len=100) :: sbcName ! south BC name
    character(len=100) :: wbcName ! west BC name
    character(len=100) :: ebcName ! east BC name
    character(len=100) :: solverName ! ice flow method name
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
  !   Note: components must be set before init
  ! --------------------------------------------------------------------------- 
  subroutine init(i)
    class(ice_type), intent(inout) :: i
  end subroutine init


  ! ---------------------------------------------------------------------------
  ! SUB: run the ice  model for a specified duration
  ! ---------------------------------------------------------------------------
  subroutine run(i)
    class(ice_type) :: i
  end subroutine run

end module ice_module
