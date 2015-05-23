! =============================================================================
! Ice exact solution for Bueler et al 2005 test A (see ref [1])
!
! Description: Test A is a steady state, isothermal ice cap with a constant,
!   positive surface ice flux and a fixed radius. The procedures in this module
!   setup and compute this steady state solution. Enabled if ice_soln_name is
!   'bueler_isothermal_a'
!
! Parameters:
!   (1) M0: constant positive balance rate within the ice cap
!   (2) L: constant fixed radius of the ice cap 
!   (3) A: constant ice deformation flow factor
!
! References:
!   [1] Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
!   L.  N. (2005). Exact solutions and verification of numerical models for
!   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
!   doi:10.3189/172756505781829449
! 
! Public: init_bueler_isothermal_a, solve_bueler_isothermal_a
!
! Private: M0, L, A, r 
!
! =============================================================================

module ice_soln_bueler_isothermal_a

use kinds, only: rp
use param, only: param_type
use state, only: state_type

implicit none
private
public :: init_bueler_isothermal_a, solve_bueler_isothermal_a

  ! ---------------------------------------------------------------------------
  ! VARS: set in init_bueler_isothermal_a
  ! ---------------------------------------------------------------------------
  real(rp) :: M0 ! constant positive surface ice flux [m_ice/a]
  real(rp) :: L ! fixed ice cap radius [m]
  real(rp) :: A ! constant ice deformation factor [Pa^-3 a^-1]
  real(rp), allocatable :: r(:,:) ! distance from corner [m]


contains


  ! ---------------------------------------------------------------------------  
  ! SUB: check parameters and initialize variables, once only
  ! ---------------------------------------------------------------------------
  subroutine init_bueler_isothermal_a(p, s)
  
    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s

    integer :: i, j

    ! expect exactly 3 parameters
    if (size(p%ice_soln_param) .ne. 3) then
      print *, 'Invalid ice solution parameters: bueler_isothermal_a requires &
               &exactly 3 parameters.'
      stop
    end if

    ! rename parameters
    M0 = p%ice_soln_param(1)
    L = p%ice_soln_param(2)
    A = p%ice_soln_param(3)

    ! M0 must be positive
    if (M0 .le. 0.0_rp) then
      print *, 'Invalid ice solution parameter: bueler_isothermal_a requires &
               &a positive M0'
      stop
    end if

    ! L must be positive
    if (L .le. 0.0_rp) then
      print *, 'Invalid ice solution parameter: bueler_isothermal_a requires &
               &a positive L'
      stop 
    end if

    ! A must be positive
    if (A .le. 0.0_rp) then
      print *, 'Invalid ice solution parameter: bueler_isothermal_a requires &
               &a positive A'
      stop 
    end if

    ! compute distance from x = 0, y = 0
    allocate(r(p%nx, p%ny))
    do j = 1, p%ny
      do i = 1, p%nx
        r(i,j) = sqrt(s%x(i)*s%x(i)+s%y(j)*s%y(j))
      end do
    end do

  end subroutine init_bueler_isothermal_a


  ! ---------------------------------------------------------------------------
  ! SUB: get solution
  ! ---------------------------------------------------------------------------
  subroutine solve_bueler_isothermal_a(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s

    integer :: i, j
    real(rp) :: c1, c2, gam

    ! pre-compute constants
    gam = 2.0_rp/5.0_rp*A*(p%rhoi*p%grav)**3.0_rp
    c1 = (4.0_rp*M0/gam)**(1.0_rp/8.0_rp)
    c2 = L**(4.0_rp/3.0_rp)

    ! solve
    where (r .lt. L) 
      s%ice_h_soln = c1*(c2-r**(4.0_rp/3.0_rp))**(3.0_rp/8.0_rp)
    elsewhere
      s%ice_h_soln = 0.0_rp
    end where

  end subroutine solve_bueler_isothermal_a


end module ice_soln_bueler_isothermal_a
