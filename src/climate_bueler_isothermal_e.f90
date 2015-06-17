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
!   (8) th2: maximum angle of sliding region [radians] 
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
  real(rp) :: M0 ! constant positive surface ice flux [m_ice/a]
  real(rp) :: Mn ! arbitrarily large negative iceflux outside icecap 
  real(rp) :: L ! ice cap radius, [m]
  real(rp) :: A ! isothermal ice deformation parameter [Pa-3 a-1]
  real(rp) :: m_max  ! maximum sliding coefficient [m/Pa/a]
  real(rp) :: r1, r2 ! min max radius of sliding areas [m]
  real(rp) :: th1, th2 ! min max angle of sliding areas [radians'
  real(rp) :: gam ! gamma, combined ice flow constant
  real(rp) :: cv ! combined constant, see [1] appendix
  real(rp), allocatable :: rr(:,:) ! distance from origin [m]
  real(rp), allocatable :: thth(:,:) ! angle from positive x-axis [radians]
  real(rp), allocatable :: ww(:,:) ! combined constant, see [1] appendix 
  real(rp), allocatable :: hhv(:,:), hhv_p(:,:), hhv_pp(:,:) ! see [1] appendix
  real(rp), allocatable :: mm(:,:), mm_p(:,:) ! see [1] appendix
  real(rp), allocatable :: compensatory(:,:) ! compensatory component of qsurf
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
    Mn = -1000.0_rp
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

    ! m_max must be positive
    if (m_max .le. 0.0_rp) then
      print *, 'Invalid climate parameter: bueler_isothermal_e requires &
               &a positive m_max'
      stop 
    end if

    ! r1 must be positive
    if (r1 .le. 0.0_rp) then
      print *, 'Invalid climate parameter: bueler_isothermal_e requires &
               &a positive r1'
      stop 
    end if

    ! r2 must be positive
    if (r2 .le. 0.0_rp) then
      print *, 'Invalid climate parameter: bueler_isothermal_e requires &
               &a positive r2'
      stop 
    end if

    ! th1 must be positive
    if (th1 .le. 0.0_rp) then
      print *, 'Invalid climate parameter: bueler_isothermal_e requires &
               &a positive th1'
      stop 
    end if

    ! th2 must be positive
    if (th2 .le. 0.0_rp) then
      print *, 'Invalid climate parameter: bueler_isothermal_e requires &
               &a positive th2'
      stop 
    end if

    ! r2 must be larger than r1
    if (r2 .le. r1) then
      print *, 'Invalid climate parameter: bueler_isothermal_e requires &
               & that r1 < r2'
      stop 
    end if

    ! th2 must be larger than th1
    if (th2 .le. th1) then
      print *, 'Invalid climate parameter: bueler_isothermal_e requires &
               & that th1 < th2'
      stop 
    end if

    ! allocate grids
    allocate(rr(p%nx, p%ny), thth(p%nx, p%ny), ww(p%nx, p%ny))
    allocate(hhv(p%nx, p%ny), hhv_p(p%nx, p%ny), hhv_pp(p%nx, p%ny))
    allocate(mm(p%nx, p%ny), mm_p(p%nx, p%ny))
    allocate(compensatory(p%nx, p%ny))

    ! coordinate grids (polar, m and radians)
    do j = 1, p%ny
      do i = 1, p%nx
        rr(i,j) = sqrt(s%x(i)*s%x(i)+s%y(j)*s%y(j))
        thth(i,j) = atan2(s%y(j), s%x(i))
        !! DEBUG
        !print *, s%x(i), s%y(j), thth(i,j)
      end do
    end do

    ! pre-compute constants 
    gam = 2.0_rp/5.0_rp*A*(p%rhoi*p%grav)**3.0_rp !#ok
    cv = (4.0_rp*M0/gam)**(1.0_rp/8.0_rp) !#ok

    ! pre-compute grids for compensatory accum (only needed in sliding areas)
    ww = 0.0_rp
    hhv = 0.0_rp
    hhv_p = 0.0_rp
    mm = 0.0_rp
    mm_p = 0.0_rp
    compensatory = 0.0_rp
    where ((rr .gt. r1) .and. (rr .lt. r2) .and. (thth .gt. th1) .and. (thth .lt. th2))
      ww = L**(4.0_rp/3.0_rp)-rr**(4.0_rp/3.0_rp) !#ok
      hhv = cv*ww**(3.0_rp/8.0_rp) !#ok
      hhv_p = -cv/2.0_rp*rr**(1.0_rp/3.0_rp)*ww**(-5.0_rp/8.0_rp) !#ok
      hhv_pp = -cv/6.0_rp*ww**(-13.0_rp/8.0_rp) * &
          (rr**(-2.0_rp/3.0_rp)*ww+5.0_rp/2.0_rp*rr**(2.0_rp/3.0_rp)) !#ok
      mm = m_max*4.0_rp*(rr-r1)*(r2-rr)/(r2-r1)**2 * & 
          4.0_rp*(thth-th1)*(th2-thth)/(th2-th1)**2 !#ok

      mm_p = m_max*4.0_rp*(r1+r2-2.0_rp*rr)/(r2-r1)**2 * & 
          4.0_rp*(thth-th1)*(th2-thth)/(th2-th1)**2 

      compensatory = -p%rhoi*p%grav*(hhv**2*hhv_p*(mm/rr+mm_p) + &
          mm*hhv*(2.0_rp*hhv_p**2+hhv*hhv_pp))

    end where


    !! DEBUG
    !! check input parameters
    !print *, 'Ad: ', A, ' 1/Pa3/a'
    !print *, 'GAMMA: ', gam, ' 1/m3/a'
    !print *, 'GAMMA: ', gam/31556926.0_rp, ' 1/m3/s'
    !print *, 'M0: ', M0, ' m/a'
    !print *, 'L: ', L, ' m' 
    !print *, 'm_max: ', m_max, ' m/a/Pa'
    !print *, 'm_max: ', m_max/31556926.0_rp, ' m/s/Pa'
    !print *, 'r1: ', r1, ' m' 
    !print *, 'r2: ', r2, ' m'  
    !print *, 'th1: ', th1, ' rad' 
    !print *, 'th1: ', th1*180.0_rp/3.1415_rp, ' degrees' 
    !print *, 'th2: ', th2, ' rad' 
    !print *, 'th2: ', th2*180.0_rp/3.1415_rp, ' degrees' 
    !! check computed values
    !print *, 'compensatory max: ', maxval(compensatory+M0), ' m/a'
    !print *, 'compensatory min: ', minval(compensatory+M0), ' m/a'
    !!print *, mm 
    !!do j = 1, p%ny
    !!  do i = 1, p%nx
    !!    print *, ''
    !!    print *, 'r: ', rr(i,j)
    !!    print *, 'th: ', thth(i,j)
    !!    print *, 'w: ', ww(i,j)
    !!    print *, 'Hvpp: ', hhv_pp(i,j)  
    !!  end do
    !!end do
    !! END DEBUG

  end subroutine init_bueler_isothermal_e


  ! ---------------------------------------------------------------------------
  subroutine update_bueler_isothermal_e(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: Get climate at current time
  ! ---------------------------------------------------------------------------

    where (rr .lt. L)
      s%ice_q_surf = M0+compensatory
    elsewhere
      s%ice_q_surf = 0.0_rp !Mn
    end where

  end subroutine update_bueler_isothermal_e

end module climate_bueler_isothermal_e
