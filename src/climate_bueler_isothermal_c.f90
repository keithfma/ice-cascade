! =============================================================================
! Climate methods for Bueler et al 2005 test C (see ref [1])
! 
! Description: Only surface ice flux is computed. Enabled if
!   climate_name is 'bueler_isothermal_c'. 
!  
! Parameters:
!   (1)
!   (2) 
!
! References:
!   [1] Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
!   L.  N. (2005). Exact solutions and verification of numerical models for
!   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
!   doi:10.3189/172756505781829449
! 
! Public: init_bueler_isothermal_c, update_bueler_isothermal_c
!
! Private: 
!
! =============================================================================

module climate_bueler_isothermal_c

use kinds, only: rp
use param, only: param_type
use state, only: state_type

implicit none
private
public :: init_bueler_isothermal_c, update_bueler_isothermal_c


  ! ---------------------------------------------------------------------------
  ! VARS: set in init_bueler_isothermal_c 
  ! ---------------------------------------------------------------------------
  real(rp), allocatable :: r(:,:) ! distance from origin [m]


contains


  ! ---------------------------------------------------------------------------
  ! SUB: check parameters and initialize variables, once only
  ! ---------------------------------------------------------------------------
  subroutine init_bueler_isothermal_c(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s

    integer :: i, j
    real(rp) :: x, y

    !! expect exactly 2 parameters
    !if (size(p%climate_param) .ne. 2) then
    !  print *, 'Invalid climate parameters: bueler_isothermal_c requires &
    !           &exactly 2 parameters.'
    !  stop
    !end if

    !! rename parameters
    !M0 = p%climate_param(1)
    !L = p%climate_param(2)
    !Mn = -1000.0_rp

    !! M0 must be positive
    !if (M0.le. 0.0_rp) then
    !  print *, 'Invalid climate parameter: bueler_isothermal_c method requires &
    !            &a positive M0'
    !  stop
    !end if

    !! L must be positive
    !if (L .le. 0.0_rp) then
    !  print *, 'Invalid climate parameter: bueler_isothermal_c method requires &
    !           &a positive L'
    !  stop 
    !end if

    !! radial distance from SW corner, ignoring ghost points, (2,2)
    !allocate(r(p%nx, p%ny))
    !do j = 1, p%ny
    !  do i = 1, p%nx
    !    x = s%x(i)-s%x(2)
    !    y = s%y(j)-s%y(2)
    !    r(i,j) = sqrt(x*x+y*y)
    !  end do
    !end do

  end subroutine init_bueler_isothermal_c


  ! ---------------------------------------------------------------------------
  ! SUB: get climate at current time
  ! ---------------------------------------------------------------------------
  subroutine update_bueler_isothermal_c(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s

    !where (r .lt. L)
    !  s%ice_q_surf = M0
    !elsewhere
    !  s%ice_q_surf = Mn
    !end where

  end subroutine update_bueler_isothermal_c


end module climate_bueler_isothermal_c

