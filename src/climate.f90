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

end module climate_mod
