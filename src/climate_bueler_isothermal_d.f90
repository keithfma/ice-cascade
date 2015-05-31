! =============================================================================
! Climate methods for Bueler et al 2005 test D (see ref [1])
!
! Description: Test D is a transient, isothermal ice cap with oscilliatory
!   surface ice flux in an annular band. The solution is the sum of a
!   steady-state case and a pertubation, designed such that the margin remains
!   fixed. The procedures in this module setup and compute this transient
!   solution. Enabled if ice_soln_name is 'bueler_isothermal_d'
!
! Parameters:
!   (1) h0: ice cap dome height, [m]
!   (2) L: ice cap radius [m]
!   (3) tp: period of pertubation, [a]
!   (4) cp: amplitude of perturbation, [m] 
!
! References:
!   [1] Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
!   L.  N. (2005). Exact solutions and verification of numerical models for
!   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
!   doi:10.3189/172756505781829449
! 
! Public: init_bueler_isothermal_d, update_bueler_isothermal_d
!
! Private: 
!
! =============================================================================

module climate_bueler_isothermal_d

use kinds, only: rp
use param, only: param_type
use state, only: state_type

implicit none
private
public :: init_bueler_isothermal_d, update_bueler_isothermal_d

  ! ---------------------------------------------------------------------------
  real(rp) :: h0 ! ice cap dome height, [m]
  real(rp) :: L ! ice cap radius, [m]
  real(rp) :: tp ! period of pertubation, [a]
  real(rp) :: cp ! amplitude of perturbation, [m] 
  !
  ! ABOUT: set in init_bueler_isothermal_d
  ! ---------------------------------------------------------------------------


contains


  ! ---------------------------------------------------------------------------  
  subroutine init_bueler_isothermal_d(p, s)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s
  !
  ! ABOUT: check parameters and initialize variables, once only
  ! ---------------------------------------------------------------------------

  end subroutine init_bueler_isothermal_d


  ! ---------------------------------------------------------------------------
  subroutine update_bueler_isothermal_d(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: Get climate at current time
  ! ---------------------------------------------------------------------------


  end subroutine update_bueler_isothermal_d


end module climate_bueler_isothermal_d

