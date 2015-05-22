! =============================================================================
! Ice exact solution for Bueler et al 2005 test B (see ref [1])
!
! Description: Test B is a transient, isothermal ice cap with a constant, zero
!   surface ice flux. The procedures in this module setup and compute this
!   transient solution. Enabled if ice_soln_name is 'bueler_isothermal_b'
!
! Parameters:
!   (1) alpha: constant exponent, [1]
!   (2) beta: constant exponent, [1]
!   (3) h0: ice cap dome height at initial time, [m]
!   (4) r0: ice cap radius at initial time, [m]
!   (5) t0: initial time, [a]
!
! References:
!   [1] Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
!   L.  N. (2005). Exact solutions and verification of numerical models for
!   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
!   doi:10.3189/172756505781829449
! 
! Public: init_bueler_isothermal_b, solve_bueler_isothermal_b
!
! Private: alpha, beta, h0 r0, t0, r
!
! =============================================================================

module ice_soln_bueler_isothermal_b

use kinds, only: rp
use param, only: param_type
use state, only: state_type

implicit none
private
public :: init_bueler_isothermal_b, solve_bueler_isothermal_b

  ! ---------------------------------------------------------------------------
  ! VARS: set in init_bueler_isothermal_a
  ! ---------------------------------------------------------------------------
  real(rp) :: alpha ! constant exponent, [1]
  real(rp) :: beta ! constant exponent, [1]
  real(rp) :: h0 ! ice cap dome height at initial time, [m]
  real(rp) :: r0 ! ice cap radius at initial time, [m]
  real(rp) :: t0 ! initial time, [a]
  real(rp), allocatable :: r(:,:) ! distance from corner [m]


contains


  ! ---------------------------------------------------------------------------  
  ! SUB: check parameters and initialize variables, once only
  ! ---------------------------------------------------------------------------
  subroutine init_bueler_isothermal_b(p, s)
  
    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s

    integer :: i, j
    real(rp) :: x, y

    ! expect exactly 5 parameters
    if (size(p%ice_soln_param) .ne. 5) then
      print *, 'Invalid ice solution parameters: bueler_isothermal_b requires &
               &exactly 5 parameters.'
      stop
    end if

    ! rename parameters
    alpha = p%ice_soln_param(1)
    beta = p%ice_soln_param(2)
    h0 = p%ice_soln_param(3)
    r0 = p%ice_soln_param(4)
    t0 = p%ice_soln_param(5)

    ! h0 must be positive
    if (h0 .le. 0.0_rp) then
      print *, 'Invalid ice solution parameter: bueler_isothermal_a requires &
               &a positive h0'
      stop
    end if
    
    ! r0 must be positive
    if (r0 .le. 0.0_rp) then
      print *, 'Invalid ice solution parameter: bueler_isothermal_a requires &
               &a positive r0'
      stop
    end if

    ! t0 must be positive
    if (t0 .le. 0.0_rp) then
      print *, 'Invalid ice solution parameter: bueler_isothermal_a requires &
               &a positive t0'
      stop
    end if

    ! radial distance from SW corner, ignoring ghost points, (2,2)
    allocate(r(p%nx, p%ny))
    do j = 1, p%ny
      do i = 1, p%nx
        x = s%x(i)-s%x(2)
        y = s%y(j)-s%y(2)
        r(i,j) = sqrt(x*x+y*y)
      end do
    end do

  end subroutine init_bueler_isothermal_b


  ! ---------------------------------------------------------------------------
  ! SUB: get solution
  ! ---------------------------------------------------------------------------
  subroutine solve_bueler_isothermal_b(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s

    real(rp) :: c1, c2, e1, e2, rm, tp

    ! compute some constants
    tp = s%now/t0 
    c1 = h0*tp**(-alpha)
    c2 = tp**(-beta)/r0
    e1 = 4.0_rp/3.0_rp
    e2 = 3.0_rp/7.0_rp
    
    ! solve
    rm = r0*tp**beta ! current margin radius
    where (r .le. rm)
      s%ice_h_soln = c1*(1-(c2*r)**e1)**e2
    elsewhere
      s%ice_h_soln = 0.0_rp
    end where

  end subroutine solve_bueler_isothermal_b

end module ice_soln_bueler_isothermal_b

