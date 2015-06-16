! =============================================================================
! Climate methods for Bueler et al 2005 test E (see ref [1])
!
! Description: Test E is a steady state ice cap with sliding in 4
!   ice-stream-like sectors and the same exact solution as in Test A. The
!   procedures in this module compute the "compensitory accumulation" required
!   to generate this solution. Enabled if climate_name is 'bueler_isothermal_e'
!
! Parameters:
!   (1) M0: constant positive balance rate within the ice cap
!   (2) L: constant fixed radius of the ice cap 
!   (3) A: constant ice deformation flow factor
!   (4) m_max: maximum value of the sliding coeffcient [m/Pa/a]
!   (5) r1: minimum radius of sliding region [m]
!   (6) r2: maximum radius of sliding region [m]
!   (7) th1: minimum angle of sliding region [radians] 
!   (8) th1: maximum angle of sliding region [radians] 
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
  real(rp) :: M0 ! 
  real(rp) :: L ! ice cap radius, [m]
  real(rp) :: A ! isothermal ice deformation parameter [Pa-3 a-1]
  real(rp) :: m_max  ! maximum sliding coefficient [m/Pa/a]
  real(rp) :: r1, r2 ! min max radius of sliding areas [m]
  real(rp) :: th1, th2 ! min max angle of sliding areas [radians'
  real(rp) :: gam ! gamma, combined ice flow constant
  real(rp) :: cv ! combined constant, see [1] appendix
  real(rp), allocatable :: rr(:,:) ! distance from origin [m]
  real(rp), allocatable :: ww(:,:) ! combined constant, see [1] appendix 
  real(rp), allocatable :: hhv(:,:), hhv_p(:,:), hhv_pp(:,:) ! see [1] appendix
  real(rp), allocatable :: mm(:,:), mm_p(:,:) ! see [1] appendix
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

    integer :: i, j

    ! ! expect exactly XXX parameters
    ! if (size(p%climate_param) .ne. XXX) then
    !   print *, 'Invalid climate parameters: bueler_isothermal_d requires &
    !            &exactly 6 parameters.'
    !   stop
    ! end if

    ! rename parameters
    M0 = p%climate_param(1)
    L = p%climate_param(2)
    A = p%climate_param(3)
    m_max = p%climate_param(4)
    r1 = p%climate_param(5)
    r2 = p%climate_param(6)
    th1 = p%climate_param(7)
    th2 = p%climate_param(8)

    ! M0 must be positive
    if (M0 .le. 0.0_rp) then
      print *, 'Invalid climate parameter: bueler_isothermal_e requires &
               &a positive M0'
      stop
    end if

    ! L must be positive
    if (L .le. 0.0_rp) then
      print *, 'Invalid climate parameter: bueler_isothermal_e requires &
               &a positive L'
      stop 
    end if

    ! A must be positive
    if (A .le. 0.0_rp) then
      print *, 'Invalid climate parameter: bueler_isothermal_e requires &
               &a positive A'
      stop 
    end if

    ! allocate grids
    allocate(rr(p%nx, p%ny))
    allocate(ww(p%nx, p%ny))

    ! pre-compute constants
    gam = 2.0_rp/5.0_rp*A*(p%rhoi*p%grav)**3.0_rp
    cv = (4.0_rp*M0/gam)**(1.0_rp/8.0_rp)

    ! compute distance from x = 0, y = 0
    do j = 1, p%ny
      do i = 1, p%nx
        rr(i,j) = sqrt(s%x(i)*s%x(i)+s%y(j)*s%y(j))
        ww(i,j) = L**(4.0_rp/3.0_rp)-rr(i,j)**(4.0_rp/3.0_rp)
      end do
    end do


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
