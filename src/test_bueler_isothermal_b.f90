! =============================================================================
! Methods for the bueler_isothermal_a test case.
!
! The methods will be invoked in the case the parameter 'ice_soln_name' is set
! to 'bueler_isothermal_b'. 
!
! Contains:
!
!   init_bueler_isothermal_b: Checks for sane parameters and sets global
!   variables. Invoked once only, then variables are saved.
!
!   ice_soln_bueler_isothermalba: Solution as described in reference (1). 
!     Requires and 5 parameters as defined in the reference:
!       ice_soln_param(1:5) = [alpha, beta, H0, R0, t0]
!
! References:
! (1) Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman, L.
!   N. (2005). Exact solutions and verification of numerical models for
!   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
!   doi:10.3189/172756505781829449
!
! ============================================================================

module test_bueler_isothermal_b_mod

use kinds_mod, only: rp
use param_mod, only: param_type
use state_mod, only: state_type

implicit none
private
public :: ice_soln_bueler_isothermal_b


  ! ---------------------------------------------------------------------------
  ! Global parameters (set once, then constant)
  ! ---------------------------------------------------------------------------
  logical, save :: set ! flag indicating if param have been set
  real(rp), save :: alpha
  real(rp), save :: beta
  real(rp), save :: H0
  real(rp), save :: R0
  real(rp), save :: t0
  real(rp), allocatable, save :: r(:,:) ! distance from corner [m]


contains


  ! ---------------------------------------------------------------------------
  ! SUB: Set global parameters, executed once
  ! ---------------------------------------------------------------------------
  subroutine init_bueler_isothermal_b(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s

    integer :: i, j

    ! define global parameters
    set = .true.
    alpha = p%ice_soln_param(1)
    beta = p%ice_soln_param(2)
    H0 = p%ice_soln_param(3)
    R0 = p%ice_soln_param(4)
    t0 = p%ice_soln_param(5)
    allocate(r(p%nx, p%ny))
    do j = 1, p%ny
      do i = 1, p%nx
        r(i,j) = sqrt(s%x(i)*s%x(i)+s%y(j)*s%y(j))
      end do
    end do

    ! check that parameters are sane

    ! H0 must be positive
    if (H0 .le. 0.0_rp) then
      print *, 'Invalid parameters: method requires a positive H0'
      stop 'Stopped.'
    end if

    ! R0 must be positive
    if (R0 .le. 0.0_rp) then
      print *, 'Invalid parameters: method requires a positive R0'
      stop 'Stopped.'
    end if

    ! t0 must be positive
    if (t0 .le. 0.0_rp) then
      print *, 'Invalid parameters: method requires a positive t0'
      stop 'Stopped.'
    end if

  end subroutine init_bueler_isothermal_b


  ! ---------------------------------------------------------------------------
  ! SUB: Ice exact solution method for this benchmark
  ! ---------------------------------------------------------------------------
  subroutine ice_soln_bueler_isothermal_b(p, s)

    type(param_type), intent(in)    :: p 
    type(state_type), intent(inout) :: s

    real(rp) :: e1, e2, Hd, Rm 

    ! initialize parameters, once
    if (.not. set) call init_bueler_isothermal_b(p, s)

    ! compute constants
    Rm = R0*(s%time_now/t0)**beta ! current ice cap radius
    Hd = H0*(s%time_now/t0)**(-alpha) ! current dome height
    e1 = 4.0_rp/3.0_rp
    e2 = 3.0_rp/7.0_rp

    where (r .lt. Rm)
      s%ice_h_soln = Hd*(1.0_rp-(r/Rm)**e1)**e2
    elsewhere
      s%ice_h_soln = 0.0_rp
    end where

  end subroutine ice_soln_bueler_isothermal_b

end module test_bueler_isothermal_b_mod
