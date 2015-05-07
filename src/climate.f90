! =============================================================================
! Climate model, including variables and procedures
!
! Contains:
!   type climate_type (public)
!   NOTE: list is not yet complete
! ============================================================================

module climate_mod

use kinds_mod, only: rp
use param_mod, only: param_type
use state_mod, only: state_type

implicit none
private
public climate_type

  ! --------------------------------------------------------------------------- 
  ! TYPE: all variables and procedures for the climate model component
  ! ---------------------------------------------------------------------------
  type climate_type
    logical                                 :: on     ! enable/disable
    procedure(update_tmpl), pointer, nopass :: update ! selected climate method
  contains
    procedure, pass                         :: init   ! initialize model object
  end type climate_type


  ! ---------------------------------------------------------------------------
  ! TEMPLATE: commom form for climate methods
  ! ---------------------------------------------------------------------------
  abstract interface
    subroutine update_tmpl(p, s) 
      import                          :: param_type, state_type
      type(param_type), intent(in)    :: p 
      type(state_type), intent(inout) :: s
    end subroutine update_tmpl
  end interface

contains

  ! ---------------------------------------------------------------------------
  ! SUB: Initilialize climate object
  ! ---------------------------------------------------------------------------
  subroutine init(c, p)

    class(climate_type), intent(inout) :: c
    type(param_type), intent(in) :: p

    ! assign climate method
    select case (p%climate_name)

    case ('none')
      c%on = .false.
      c%update => NULL() ! will fail if called

    case default
      print *, 'Invalid parameter: climate method name is not recognized.'
      stop 'Stopped.'

    end select

  end subroutine init

  !  ! Set surface temperature procedure
  !  select case (c%name_temp_surf)
  !    case ("constant")
  !      c%get_temp_surf => temp_constant
  !    case default
  !      print *, "Invalid name for surface temperature model name: ", &
  !               trim(c%name_temp_surf)
  !      stop -1
  !  end select

  !  ! Set precipitation procedure
  !  select case (c%name_precip)
  !    case ("constant")
  !      c%get_precip => precip_constant
  !    case default
  !      print *, "Invalid name for precipitation model name: ", &
  !               trim(c%name_precip)
  !      stop -1
  !  end select

  !  ! Set surface ice flux procedure
  !  select case (c%name_ice_q_surf)
  !    case ("constant")
  !      c%get_ice_q_surf => ice_q_surf_constant
  !    case default
  !      print *, "Invalid name for surface ice flux model name: ", &
  !               trim(c%name_ice_q_surf)
  !      stop -1
  !  end select

  !  ! Set runoff procedure
  !  select case (c%name_runoff)
  !    case ("constant")
  !      c%get_runoff => runoff_constant
  !    case default
  !      print *, "Invalid name for runoff model name: ", &
  !               trim(c%name_runoff)
  !      stop -1
  !  end select

  !end subroutine init


  !! ---------------------------------------------------------------------------
  !! SUB: Update climate mode state
  !! ---------------------------------------------------------------------------
  !subroutine update(c, s)

  !  class(climate_type), intent(in) :: c
  !  type(state_type), intent(inout) :: s

  !  if (c%on_temp_surf) call c%get_temp_surf(s)
  !  if (c%on_precip) call c%get_precip(s)
  !  if (c%on_ice_q_surf) call c%get_ice_q_surf(s)
  !  if (c%on_runoff) call c%get_runoff(s)

  !end subroutine update


  !! ---------------------------------------------------------------------------
  !! SUB: Surface temperature model, constant in space and time
  !!   Parameters:
  !!     c%param_temp_surf(1) = temperature, [C]
  !!     all others unused.
  !! ---------------------------------------------------------------------------
  !subroutine temp_constant(c, s) 

  !  class(climate_type), intent(in) :: c 
  !  type(state_type), intent(inout) :: s
  !
  !  s%temp_surf = c%param_temp_surf(1)    

  !end subroutine temp_constant 


  !! ---------------------------------------------------------------------------
  !! SUB: Precipitation model, constant in space and time
  !!   Parameters:
  !!     c%param_precip(1) = precip rate, [m_water/a]
  !!     all others unused.
  !! ---------------------------------------------------------------------------
  !subroutine precip_constant(c, s) 

  !  class(climate_type), intent(in) :: c 
  !  type(state_type), intent(inout) :: s
  !
  !  s%precip = c%param_precip(1)    

  !end subroutine precip_constant 


  !! ---------------------------------------------------------------------------
  !! SUB: Surface ice flux model, constant in space and time
  !!   Parameters:
  !!     c%param_ice_q_surf(1) = flux, [m_ice/a] 
  !!     all others unused.
  !! ---------------------------------------------------------------------------
  !subroutine ice_q_surf_constant(c, s) 

  !  class(climate_type), intent(in) :: c 
  !  type(state_type), intent(inout) :: s
  !
  !  s%ice_q_surf = c%param_ice_q_surf(1)    

  !end subroutine ice_q_surf_constant 


  !! ---------------------------------------------------------------------------
  !! SUB: Runoff model, constant in space and time
  !!   Parameters:
  !!     c%param_runoff(1) = runoff rate, [m_water/a]
  !!     all others unused.
  !! ---------------------------------------------------------------------------
  !subroutine runoff_constant(c, s) 

  !  class(climate_type), intent(in) :: c 
  !  type(state_type), intent(inout) :: s
  !
  !  s%runoff = c%param_runoff(1)    

  !end subroutine runoff_constant 

end module climate_mod
