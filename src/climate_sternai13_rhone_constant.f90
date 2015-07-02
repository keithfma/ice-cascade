! =============================================================================
! Climate method
!
! Description: Climate as specified for glacier models of the Rhone valley in
!   [1]. Sets surface temperature and surface ice flux only. Temperature is
!   defined by sea-level temperature and a linear lapse rate, and surface ice
!   flux is linearly proportional to temperature then capped at specified
!   minimum and maximum values. In this "constant" variant, sea-level
!   temperature is constant in time. 
!
! Parameters:
!   (1) T0 = sea-level temperature [C]
!   (2) lam = temperature lapse rate [C/m]
!   (3) gam = iceflux-temperature conversion constant [m_ice/C]
!   (4) qmin = minimum surface ice flux [m_ice/a]
!   (5) qmax = maximum surface ice flux [m_ice/a]

! References:
!   [1] Sternai, P., Herman, F., Valla, P. G., & Champagnac, J.-D. (2013).
!   Spatial and temporal variations of glacial erosion in the Rhône valley
!   (Swiss Alps): Insights from numerical modeling. Earth and Planetary
!   Science Letters, 368, 119-131. doi:10.1016/j.epsl.2013.02.039
!
! Public: init_sternai13_rhone_constant, update_sternai13_rhone_constant
! 
! =============================================================================

module climate_sternai13_rhone_constant

use kinds, only: rp
use param, only: param_type
use state, only: state_type

implicit none
private
public :: init_sternai13_rhone_constant, update_sternai13_rhone_constant

  ! ---------------------------------------------------------------------------
  real(rp) :: T0 ! sea-level temperature [C]
  real(rp) :: lam ! temperature lapse rate [C/m]
  real(rp) :: gam ! iceflux-temperature conversion constant [m_ice/C]
  real(rp) :: qmin, qmax ! minimum/maximum surface ice flux [m_ice_a]
  !
  ! ABOUT: Local parameters, set in init_sternai13_rhone_constant
  ! ---------------------------------------------------------------------------

contains

  ! ---------------------------------------------------------------------------  
  subroutine init_sternai13_rhone_constant(p, s)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s
  !
  ! ABOUT: check parameters and initialize variables, once only
  ! ---------------------------------------------------------------------------

    ! expect exactly 5 parameters
    if (size(p%climate_param) .ne. 5) then
      print *, 'Invalid climate parameters: sternai13_rhone_constant requires &
               &exactly 5 parameters.'
      stop
    end if

    ! rename parameters
    T0 = p%climate_param(1)
    lam = p%climate_param(2)
    gam = p%climate_param(3)
    qmin = p%climate_param(4)
    qmax = p%climate_param(5)

    ! qmin must be less then qmax
    if (qmin .ge. qmax) then
      print *, 'Invalid climate parameters: sternai13_rhone_constant requires &
                &qmin be less than qmax'
      stop
    end if


  end subroutine init_sternai13_rhone_constant


  ! ---------------------------------------------------------------------------
  subroutine update_sternai13_rhone_constant(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: Get climate at current time
  ! ---------------------------------------------------------------------------

  end subroutine update_sternai13_rhone_constant


end module climate_sternai13_rhone_constant
