! =============================================================================
! Climate methods for Bueler et al 2005 test E (see ref [1])
!
! Description: Test E is a steady state ice cap with sliding in 4
!   ice-stream-like sectors and the same exact solution as in Test A. The
!   procedures in this module compute the "compensitory accumulation" required
!   to generate this solution. Enabled if climate_name is 'bueler_isothermal_e'
!
! Parameters:
!   (1) 
!
! References:
!   [1] Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
!   L.  N. (2005). Exact solutions and verification of numerical models for
!   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
!   doi:10.3189/172756505781829449
! 
! Public: init_bueler_isothermal_e, update_bueler_isothermal_e
!
! Private: 
!
! =============================================================================

module climate_bueler_isothermal_e

use kinds, only: rp
use param, only: param_type
use state, only: state_type

implicit none
private
public :: init_bueler_isothermal_e, update_bueler_isothermal_e

  ! ---------------------------------------------------------------------------
  !
  ! ABOUT: set in init_bueler_isothermal_e
  ! ---------------------------------------------------------------------------


contains


  ! ---------------------------------------------------------------------------  
  subroutine init_bueler_isothermal_e(p, s)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s
  !
  ! ABOUT: check parameters and initialize variables, once only
  ! ---------------------------------------------------------------------------

  end subroutine init_bueler_isothermal_e

  ! ---------------------------------------------------------------------------
  subroutine update_bueler_isothermal_e(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: Get climate at current time
  ! ---------------------------------------------------------------------------

  end subroutine update_bueler_isothermal_e

end module climate_bueler_isothermal_e
