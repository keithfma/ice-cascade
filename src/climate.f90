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

    ! Set precipitation procedure
    select case (cl%name_precip)
      case ("constant")
        cl%get_precip => temp_constant
      case default
        print *, "Invalid name for precipitation model name: ", &
                 trim(cl%name_precip)
        stop -1
    end select

    ! Set surface ice flux procedure
    select case (cl%name_ice_q_surf)
      case ("constant")
        cl%get_ice_q_surf => temp_constant
      case default
        print *, "Invalid name for surface ice flux model name: ", &
                 trim(cl%name_ice_q_surf)
        stop -1
    end select

    ! Set runoff procedure
    select case (cl%name_runoff)
      case ("constant")
        cl%get_runoff => temp_constant
      case default
        print *, "Invalid name for runoff model name: ", &
                 trim(cl%name_runoff)
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


  ! ---------------------------------------------------------------------------
  ! SUB: Precipitation model, constant in space and time
  !   Parameters:
  !     cl%param_precip(1) = precip rate, [m_water/a]
  !     all others unused.
  ! ---------------------------------------------------------------------------
  subroutine precip_constant(cl, c) 

    class(climate_type), intent(in) :: cl 
    type(common_type), intent(inout) :: c
  
    c%precip = cl%param_precip(1)    

  end subroutine precip_constant 


  ! ---------------------------------------------------------------------------
  ! SUB: Surface ice flux model, constant in space and time
  !   Parameters:
  !     cl%param_ice_q_surf(1) = flux, [m_ice/a] 
  !     all others unused.
  ! ---------------------------------------------------------------------------
  subroutine ice_q_surf_constant(cl, c) 

    class(climate_type), intent(in) :: cl 
    type(common_type), intent(inout) :: c
  
    c%ice_q_surf = cl%param_ice_q_surf(1)    

  end subroutine ice_q_surf_constant 


  ! ---------------------------------------------------------------------------
  ! SUB: Runoff model, constant in space and time
  !   Parameters:
  !     cl%param_runoff(1) = runoff rate, [m_water/a]
  !     all others unused.
  ! ---------------------------------------------------------------------------
  subroutine runoff_constant(cl, c) 

    class(climate_type), intent(in) :: cl 
    type(common_type), intent(inout) :: c
  
    c%runoff = cl%param_runoff(1)    

  end subroutine runoff_constant 

end module climate_mod
