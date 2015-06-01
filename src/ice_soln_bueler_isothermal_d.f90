! =============================================================================
! Ice exact solution for Bueler et al 2005 test D (see ref [1])
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
! Public: init_bueler_isothermal_d, solve_bueler_isothermal_d
!
! Private: h0, L, tp, cp, rr, ss, hs
!
! =============================================================================

module ice_soln_bueler_isothermal_d

use kinds, only: rp
use param, only: param_type
use state, only: state_type

implicit none
private
public :: init_bueler_isothermal_d, solve_bueler_isothermal_d

  ! ---------------------------------------------------------------------------
  real(rp) :: h0 ! ice cap dome height, [m]
  real(rp) :: L ! ice cap radius, [m]
  real(rp) :: tp ! period of pertubation, [a]
  real(rp) :: cp ! amplitude of perturbation, [m] 
  real(rp), allocatable :: rr(:,:) ! distance from origin [m]
  real(rp), allocatable :: ss(:,:) ! normalized distance from origin, [m]
  real(rp), allocatable :: hs(:,:) ! steady component of solution, [m]
  real(rp), allocatable :: pp(:,:) ! transient component of solution, [m]
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

    ! expect exactly 4 parameters
    if (size(p%ice_soln_param) .ne. 4) then
      print *, 'Invalid ice solution parameters: bueler_isothermal_d requires &
               &exactly 4 parameters.'
      stop
    end if

    ! rename parameters
    h0 = p%ice_soln_param(1)
    L = p%ice_soln_param(2)
    cp = p%ice_soln_param(3)
    tp = p%ice_soln_param(4)

    ! h0 must be positive
    if (h0 .le. 0.0_rp) then
      print *, 'Invalid ice solution parameter: bueler_isothermal_d requires &
               &a positive h0'
      stop
    end if

    ! L must be positive
    if (L .le. 0.0_rp) then
      print *, 'Invalid ice solution parameter: bueler_isothermal_d requires &
               &a positive L'
      stop
    end if

    ! tp must be positive
    if (tp .le. 0.0_rp) then
      print *, 'Invalid ice solution parameter: bueler_isothermal_d requires &
               &a positive tp'
      stop
    end if

    ! allocate grids
    allocate(rr(p%nx, p%ny), ss(p%nx, p%ny), hs(p%nx, p%ny), pp(p%nx, p%ny))

    ! compute distance from x = 0, y = 0
    do j = 1, p%ny
      do i = 1, p%nx
        rr(i,j) = sqrt(s%x(i)*s%x(i)+s%y(j)*s%y(j))
        ss(i,j) = rr(i,j)/L
      end do
    end do

    ! compute steady component of ice thickness solution
    where (ss .lt. 1.0_rp)
      hs = h0/(2.0_rp/3.0_rp)**(3.0_rp/8.0_rp)*&
           (4.0_rp/3.0_rp*ss - 1.0_rp/3.0_rp + (1.0_rp-ss)**(4.0_rp/3.0_rp) - &
           ss**(4.0_rp/3.0_rp))**(3.0_rp/8.0_rp)
    elsewhere
      hs = 0.0_rp
    end where

  end subroutine init_bueler_isothermal_d


  ! ---------------------------------------------------------------------------
  subroutine solve_bueler_isothermal_d(p, s)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: get solution
  ! ---------------------------------------------------------------------------

  real(rp) :: pi

  pi = 4.0_rp*atan(1.0_rp)

  ! compute unsteady component of ice thickness
  where ((ss .gt. 0.3_rp) .and. (ss .lt. 0.9_rp))
    pp = cp*sin(2.0_rp*pi*s%now/tp)*cos(pi*(rr-0.6_rp*L)/(0.6_rp*L))**2.0_rp
  elsewhere
    pp = 0.0_rp
  end where
  
  ! combine steady and unsteady components of the solution
  s%ice_h_soln = hs+pp

  end subroutine solve_bueler_isothermal_d

end module ice_soln_bueler_isothermal_d
