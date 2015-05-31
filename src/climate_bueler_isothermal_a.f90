! =============================================================================
! Climate methods for Bueler et al 2005 test A (see ref [1])
! 
! Description: Only surface ice flux is computed, this is set to a constant
!   positive value within a fixed radius of the SW corner of the model domain
!   (1,1), and to an arbitraily large negative value outside of this radius.
!   This simulates the fixed-margin specified in the test case. Enabled if
!   climate_name is 'bueler_isothermal_a'. 
!  
! Parameters:
!   (1) M0: constant positive balance rate within the ice cap
!   (2) L: constant fixed radius of the ice cap 
!
! References:
!   [1] Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
!   L.  N. (2005). Exact solutions and verification of numerical models for
!   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
!   doi:10.3189/172756505781829449
! 
! Public: init_bueler_isothermal_a, update_bueler_isothermal_a
!
! Private: M0, L Mn, r
!
! =============================================================================

module climate_bueler_isothermal_a

use kinds, only: rp
use param, only: param_type
use state, only: state_type

implicit none
private
public :: init_bueler_isothermal_a, update_bueler_isothermal_a


  ! ---------------------------------------------------------------------------
  real(rp) :: M0 ! constant positive surface ice flux [m_ice/a]
  real(rp) :: L ! fixed ice cap radius [m]
  real(rp) :: Mn ! arbitrarily large negative iceflux outside icecap 
  real(rp), allocatable :: r(:,:) ! distance from origin [m]
  !
  ! ABOUT: reusable variables, set in init_bueler_isothermal_a 
  ! ---------------------------------------------------------------------------


contains


  ! ---------------------------------------------------------------------------
  subroutine init_bueler_isothermal_a(p, s)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s
  !
  ! ABOUT: check parameters and initialize variables, once only
  ! ---------------------------------------------------------------------------

    integer :: i, j

    ! expect exactly 2 parameters
    if (size(p%climate_param) .ne. 2) then
      print *, 'Invalid climate parameters: bueler_isothermal_a requires &
               &exactly 2 parameters.'
      stop
    end if

    ! rename parameters
    M0 = p%climate_param(1)
    L = p%climate_param(2)
    Mn = -1000.0_rp

    ! M0 must be positive
    if (M0.le. 0.0_rp) then
      print *, 'Invalid climate parameter: bueler_isothermal_a method requires &
                &a positive M0'
      stop
    end if

    ! L must be positive
    if (L .le. 0.0_rp) then
      print *, 'Invalid climate parameter: bueler_isothermal_a method requires &
               &a positive L'
      stop 
    end if

    ! compute distance from x = 0, y = 0
    do j = 1, p%ny
      do i = 1, p%nx
        r(i,j) = sqrt(s%x(i)*s%x(i)+s%y(j)*s%y(j))
      end do
    end do

  end subroutine init_bueler_isothermal_a


  ! ---------------------------------------------------------------------------
  subroutine update_bueler_isothermal_a(p, s)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! SUB: get climate at current time
  ! ---------------------------------------------------------------------------

    where (r .lt. L)
      s%ice_q_surf = M0
    elsewhere
      s%ice_q_surf = Mn
    end where

  end subroutine update_bueler_isothermal_a


end module climate_bueler_isothermal_a

