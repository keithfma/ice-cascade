! =============================================================================
! Climate methods for Bueler et al 2005 test D (see ref [1])
!
! Description: Test D is a transient, isothermal ice cap with oscilliatory
! surface ice flux in an annular band. The solution is the sum of a steady-state
! case and a pertubation, designed such that the margin remains fixed. The
! procedures in this module setup and compute the surface ice flux for this
! transient solution. Enabled if climate_name is 'bueler_isothermal_d'
!
! Parameters:
!   (1) h0: ice cap dome height, [m]
!   (2) L: ice cap radius [m]
!   (3) tp: period of pertubation, [a]
!   (4) cp: amplitude of perturbation, [m] 
!   (5) q0: surface ice flux beyond the exact margin, [m/a]
!   (6) A: isothermal ice deformation parameter [Pa-3 a-1] 
!
! References:
!   [1] Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
!   L.  N. (2005). Exact solutions and verification of numerical models for
!   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
!   doi:10.3189/172756505781829449
! 
! Public: init_bueler_isothermal_d, update_bueler_isothermal_d
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
  real(rp) :: A ! isothermal ice deformation parameter [Pa-3 a-1]
  real(rp) :: q0 ! surface ice flux beyond the exact margin, [m/a]
  real(rp) :: gam ! gamma, combined ice flow constant
  real(rp) :: pi ! pi, duh
  real(rp), allocatable :: rr(:,:) ! distance from origin [m]
  real(rp), allocatable :: ss(:,:) ! normalized distance from origin, [m]
  real(rp), allocatable :: qqs(:,:) ! steady component of surf ice flux, [m/a]
  real(rp), allocatable :: qqc(:,:) ! unsteady component of surf ice flux, [m/a]
  real(rp), allocatable :: gg(:,:), gg_p(:,:), gg_pp(:,:) ! see [1], appendix
  real(rp), allocatable :: xx(:,:), xx_p(:,:), xx_pp(:,:) ! see [1], appendix
  real(rp), allocatable :: hhs(:,:), hhs_p(:,:), hhs_pp(:,:) ! see [1], appendix
  real(rp), allocatable :: hhp(:,:), hhp_p(:,:), hhp_pp(:,:) ! see [1], appendix
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

    integer :: i, j
    real(rp) :: C 

    ! expect exactly 6 parameters
    if (size(p%climate_param) .ne. 6) then
      print *, 'Invalid climate parameters: bueler_isothermal_d requires &
               &exactly 6 parameters.'
      stop
    end if

    ! rename parameters
    h0 = p%climate_param(1)
    L = p%climate_param(2)
    cp = p%climate_param(3)
    tp = p%climate_param(4)
    q0 = p%climate_param(5)
    A = p%climate_param(6)

    ! h0 must be positive
    if (h0 .le. 0.0_rp) then
      print *, 'Invalid climate parameter: bueler_isothermal_d requires &
               &a positive h0'
      stop
    end if

    ! L must be positive
    if (L .le. 0.0_rp) then
      print *, 'Invalid climate parameter: bueler_isothermal_d requires &
               &a positive L'
      stop
    end if

    ! tp must be positive
    if (tp .le. 0.0_rp) then
      print *, 'Invalid climate parameter: bueler_isothermal_d requires &
               &a positive tp'
      stop
    end if

    ! A must be positive
    if (A .le. 0.0_rp) then
      print *, 'Invalid climate parameter: bueler_isothermal_d requires &
               &a positive A'
      stop 
    end if

    ! allocate grids
    allocate(rr(p%nx, p%ny))
    allocate(ss(p%nx, p%ny))
    allocate(qqs(p%nx, p%ny))
    allocate(qqc(p%nx, p%ny)) 
    allocate(gg(p%nx, p%ny), gg_p(p%nx, p%ny), gg_pp(p%nx, p%ny))
    allocate(xx(p%nx, p%ny), xx_p(p%nx, p%ny), xx_pp(p%nx, p%ny))
    allocate(hhs(p%nx, p%ny), hhs_p(p%nx, p%ny), hhs_pp(p%nx, p%ny))
    allocate(hhp(p%nx, p%ny), hhp_p(p%nx, p%ny), hhp_pp(p%nx, p%ny))

    ! compute distance from x = 0, y = 0
    do j = 1, p%ny
      do i = 1, p%nx
        rr(i,j) = sqrt(s%x(i)*s%x(i)+s%y(j)*s%y(j))
        ss(i,j) = rr(i,j)/L
      end do
    end do
  
    ! constants
    pi = 4.0_rp*atan(1.0_rp)
    gam = 2.0_rp/5.0_rp*A*(p%rhoi*p%grav)**3 

    ! steady component of surface ice flux
    C = gam*(h0**8)/(4.0_rp/3.0_rp*L)**3

    where (ss .eq. 0.0_rp)
      qqs = 2.0_rp*C/L
    end where
  
    where ((ss .gt. 0.0_rp) .and. (ss .lt. 1.0_rp))
      qqs = C/(L*ss)* &
            (ss**(1.0_rp/3.0_rp)+(1.0_rp-ss)**(1.0_rp/3.0_rp)-1.0_rp)**2 * &
            (2.0_rp*ss**(1.0_rp/3.0_rp)+(1.0_rp-ss)**(-2.0_rp/3.0_rp)*(1.0_rp-2.0_rp*ss)-1.0_rp)
    end where
    
    where (ss .eq. 1.0_rp)
      qqs = -C/L
    end where

    where (ss .gt. 1.0_rp)
      qqs = q0 
    end where

    ! constant parts of the unsteady component of surface ice flux

    gg = cos(pi*(rr-0.6_rp/L)/(0.6_rp*L))**2

    gg_p = -pi/(0.6_rp*L)*sin(pi*(rr-0.6_rp*L)/(0.3_rp*L))

    gg_pp = (-pi**2)/(0.18_rp*L**2)*cos(pi*(rr-0.6_rp*L)/(0.3_rp*L))

    xx = 0.0_rp
    where (ss .lt. 1.0_rp)
      xx = 4.0_rp/3.0_rp*ss-1.0_rp/3.0_rp+(1.0_rp-ss)**(4.0_rp/3.0_rp) - &
           ss**(4.0_rp/3.0_rp)
    end where

    xx_p = 0.0_rp
    where (ss .lt. 1.0_rp)
      xx_p = -4.0_rp/(3.0_rp*L)*(ss**(1.0_rp/3.0_rp) + &
             (1.0_rp-ss)**(1.0_rp/3.0_rp)-1.0_rp)
    end where

    xx_pp = 0.0_rp
    where (ss .lt. 1.0_rp)
      xx_pp = -4.0_rp/(9.0_rp*L**2)*(ss**(-2.0_rp/3.0_rp) - &
              (1.0_rp-ss)**(-2.0_rp/3.0_rp))
    end where

    hhs = h0*(2.0_rp/3.0_rp)**(-3.0_rp/8.0_rp)*xx**(3.0_rp/8.0_rp)

    hhs_p = 0.0_rp
    where (xx .gt. 0.0_rp)
      hhs_p = (3.0_rp*h0)/(8.0_rp*(2.0_rp/3.0_rp)**(3.0_rp/8.0_rp)) * &
              xx**(-5.0_rp/8.0_rp)*xx_p
    end where

    hhs_pp = 0.0_rp
    where (xx .gt. 0.0_rp)
      hhs_pp = (3.0_rp*h0)/(8.0_rp*(2.0_rp/3.0_rp)**(3.0_rp/8.0_rp)) * &
               ((-5.0_rp/8.0_rp)*xx**(-13.0_rp/8.0_rp)*xx_p**2 + &
               xx**(-5.0_rp/8.0_rp)*xx_pp)
    end where

  end subroutine init_bueler_isothermal_d


  ! ---------------------------------------------------------------------------
  subroutine update_bueler_isothermal_d(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: Get climate at current time
  ! ---------------------------------------------------------------------------

    real(rp) :: sinval, t

    ! constants
    t = s%now
    sinval = sin(2.0_rp*pi*t/tp)

    ! compute unsteady component of surface ice flux
    hhp = hhs
    where ((0.3_rp .lt. ss) .and. (ss .lt. 0.9_rp))
      hhp = hhs+cp*sinval*cos(pi*(rr-0.6_rp*L)/(0.6_rp*L))**2
    end where

    hhp_p = hhs_p+cp*sinval*gg_p

    hhp_pp = hhs_pp+cp*sinval*gg_pp

    qqc = 0.0_rp
    where ((0.3_rp .lt. ss) .and. (ss .lt. 0.9_rp))
      qqc = 2.0_rp*pi*cp/tp*gg*cos(2.0_rp*pi*t/tp) - qqs - &
            gam*(hhp**4)*(hhp_p**2)*(1.0_rp/rr*hhp*hhp_p + &
            5.0_rp*(hhp_p**2)+3.0_rp*hhp*hhp_pp)
    end where

    ! combine steady and unsteady components of surface ice flux
    s%ice_q_surf = qqs+qqc

  end subroutine update_bueler_isothermal_d


end module climate_bueler_isothermal_d

