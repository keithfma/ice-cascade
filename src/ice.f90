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
    logical :: write_vs ! output flag, ice sliding veolcity
    logical :: write_ero ! output flat, erosion rate
    integer :: nx ! num grid pts in x-dir, [1]
    integer :: ny ! num grid pts in y-dir, [1]
    real(dp) :: c_b ! ice defm coeff, [Pa-3 a-1]
    real(dp) :: c_bs ! ice sliding coeff, [Pa-3 a-1 m-2]
    real(dp) :: c_nstress_base ! basal normal stress coeff, [1]
    real(dp) :: c_constr ! constriction factor coeff, [m]
    real(dp) :: c_heat_q_base ! basal heat flux, [W m-2]
    real(dp) :: c_heat_k_ice ! ice conductivity, [W m-1 K-1]
    real(dp) :: c_ero ! glacial erosion coeff [?]
    character(len=100) :: nbcName ! north BC name
    character(len=100) :: sbcName ! south BC name
    character(len=100) :: wbcName ! west BC name
    character(len=100) :: ebcName ! east BC name
    character(len=100) :: solnName ! exact soln name
    procedure(bc), pointer, nopass :: nbc ! set north BC
    procedure(bc), pointer, nopass :: sbc ! set south BC
    procedure(bc), pointer, nopass :: wbc ! set west BC
    procedure(bc), pointer, nopass :: ebc ! set east BC
    procedure(soln), pointer, pass :: solve ! compute soln
!  contains
!    procedure, pass :: init ! initialize all components
!    procedure, pass :: run ! run model                   
  end type ice_type


  ! ---------------------------------------------------------------------------
  ! SUB TEMPLATE: common form for the bc functions
  ! ---------------------------------------------------------------------------
  abstract interface
    subroutine bc()
      import :: dp ! use special types
    end subroutine bc 
  end interface

  ! ---------------------------------------------------------------------------
  ! SUB TEMPLATE: common form for exact solutions
  ! ---------------------------------------------------------------------------
  abstract interface
    subroutine soln(i)
      import :: dp, ice_type ! use special types
      class(ice_type), intent(inout) :: i
    end subroutine soln
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
