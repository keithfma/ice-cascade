! =============================================================================
! Climate model, including variables and procedures
!
! Contains:
!   type climate_type (public)
!   NOTE: list is not yet complete
! ============================================================================

module climate_mod

use kinds_mod, only: rp
use common_mod, only: common_type

implicit none
private
public climate_type

  ! --------------------------------------------------------------------------- 
  ! TYPE: all variables and procedures for the climate model component
  ! ---------------------------------------------------------------------------
  type climate_type
    logical :: on_temp_surf ! enable/disable temperature
    logical :: on_precip ! enable/disable precipitation
    logical :: on_ice_q_surf ! enable/disable surface ice flux
    logical :: on_runoff ! enable/disable runoff
    character(len=100) :: name_temp_surf ! temperature model name
    character(len=100) :: name_precip ! precipitation model name
    character(len=100) :: name_ice_q_surf ! surface ice flux model name
    character(len=100) :: name_runoff ! runoff model name
    real(rp), dimension(10) :: param_temp_surf ! temperature model param, [various]
    real(rp), dimension(10) :: param_precip !  model param, [various]
    real(rp), dimension(10) :: param_ice_q_surf !  model param, [various]
    real(rp), dimension(10) :: param_runoff !  model param, [various]
    procedure(tpir), pointer, pass :: get_temp_surf ! compute surface temperature
    procedure(tpir), pointer, pass :: get_precip ! compute 
    procedure(tpir), pointer, pass :: get_ice_q_surf ! compute 
    procedure(tpir), pointer, pass :: get_runoff ! compute 
  contains
    procedure, pass :: init
    procedure, pass :: update
  end type climate_type


  ! ---------------------------------------------------------------------------
  ! SUB TEMPLATE: common form for surface temperature, precipitation, and
  !   surface ice flux subroutines
  ! ---------------------------------------------------------------------------
  abstract interface
    subroutine tpir(cl, c) 
      import :: climate_type, common_type ! use special types
      class(climate_type), intent(in) :: cl 
      type(common_type), intent(inout) :: c
    end subroutine tpir 
  end interface

contains

  ! ---------------------------------------------------------------------------
  ! SUB: Initilialize climate object
  ! ---------------------------------------------------------------------------
  subroutine init(cl)

    class(climate_type), intent(inout) :: cl

    ! Set surface temperature procedure
    select case (cl%name_temp_surf)
      case ("constant")
        cl%get_temp_surf => temp_constant
      case default
        print *, "Invalid name for surface temperature model name: ", &
                 trim(cl%name_temp_surf)
        stop -1
    end select

  end subroutine init

  ! ---------------------------------------------------------------------------
  ! SUB: Update climate mode state
  ! ---------------------------------------------------------------------------
  subroutine update(cl, c)

    class(climate_type), intent(in) :: cl
    type(common_type), intent(inout) :: c

    if (cl%on_temp_surf) call cl%get_temp_surf(c)

  end subroutine update

  ! ---------------------------------------------------------------------------
  ! SUB: Surface temperature model, constant in space and time
  !   Parameters:
  !     cl%param_temp_surf(1) = temperature, [C]
  !     all others unused.
  ! ---------------------------------------------------------------------------
  subroutine temp_constant(cl, c) 

    class(climate_type), intent(in) :: cl 
    type(common_type), intent(inout) :: c
  
    c%temp_surf = cl%param_temp_surf(1)    

  end subroutine temp_constant 

end module climate_mod
