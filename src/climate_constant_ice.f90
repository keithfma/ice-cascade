! =============================================================================
! Climate method
!
! Description: Constant surface ice flux set using a single input parameter, all
!   other climate variables are ignored. Invoked if climate_name is set to
!   constant_ice.
!
! Parameters:
!   (1) constant (in space and time) surface ice flux [m_ice/a]
!
! Public: init_constant_ice
!
! Private: None
! 
! =============================================================================

module climate_constant_ice

use kinds, only: rp
use param, only: param_type
use state, only: state_type

implicit none
private
public :: init_constant_ice 

contains

  ! ---------------------------------------------------------------------------
  subroutine init_constant_ice(p, s)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: check parameters and initialize variables, only once
  ! ---------------------------------------------------------------------------

    ! expect exactly 1 parameter
    if (size(p%climate_param) .ne. 1) then
      print *, 'Invalid climate parameters: constant_ice requires &
               &exactly 1 parameter.'
      stop
    end if

    ! set state variable, which will remain constant thereafter
    s%ice_q_surf = p%climate_param(1)

  end subroutine init_constant_ice

end module climate_constant_ice
