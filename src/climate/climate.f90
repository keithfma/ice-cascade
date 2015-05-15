! =============================================================================
! Climate model component for ice-cascade
!
! Public:
!   on_climate: logical, enable/disable model component
!   init_climate: procedue, intialize procedures and vars
!   update_climate: procedure, update climate variables using selected method
!
! Private: none
!   
! ============================================================================

module climate 

use kinds, only: rp
use param, only: param_type
use state, only: state_type

implicit none
private
public :: on_climate, init_climate, update_climate


  ! ---------------------------------------------------------------------------
  ! TEMPLATE: commom form for climate methods
  ! ---------------------------------------------------------------------------
  abstract interface
    subroutine update_tmpl(p, s) 
      import :: param_type, state_type
      type(param_type), intent(in) :: p 
      type(state_type), intent(inout) :: s
    end subroutine update_tmpl
  end interface


  ! ---------------------------------------------------------------------------
  ! PROCEDURES & VARS: set in init_climate
  ! ---------------------------------------------------------------------------
  logical :: on_climate ! enable/disable model
  procedure(update_tmpl), pointer :: update_climate ! selected climate model

contains

  ! ---------------------------------------------------------------------------
  ! SUB: initialize procedures and vars
  ! ---------------------------------------------------------------------------
  subroutine init_climate(p)

    type(param_type), intent(in) :: p

    ! select update procedure
    select case (p%climate_name)
    case ('none')
      on_climate = .false.
      update_climate => NULL() ! will seg-fault if called, by design
    case default
      print *, 'Invalid name for climate method' // trim(p%climate_name)
      stop 
    end select

  end subroutine init_climate

end module climate
