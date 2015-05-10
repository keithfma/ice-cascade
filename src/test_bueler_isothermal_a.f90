! =============================================================================
! Methods for the bueler_isothermal_a test case.
!
! The methods will be invoked in the case the parameter climate_name  and
! ice_soln_name are both is set to 'bueler_isothermal_a'. 
!
! Contains:
!
!   init_bueler_isothermal_a: Checks for sane parameters and sets global
!   variables. Invoked once by either climate or ice init() routines.
!
!   climate_bueler_isothermal_a: Surface ice flux is set to a constant (M0 =
!     climate_param(1)) within a fixed radius from the ice cap center (L =
!     climate_param(2)), and to an arbitrarily large negative number outside of
!     this radius.
!  
!   ice_soln_bueler_isothermal_a: Solution as described in reference (1). 
!     Requires and 3 parameters: M0, the constant positive balance rate within
!     the ice cap, L, the constant fixed radius of the ice cap, and A, the
!     constant ice deformation flow factor. These 3 parameters are elements 1,
!     2, 3 in ice_soln_param, respectively. 
!
! References:
! (1) Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman, L.
! N. (2005). Exact solutions and verification of numerical models for isothermal
! ice sheets. Journal of Glaciology, 51(173), 291-306.
! doi:10.3189/172756505781829449
!
! ============================================================================

module test_bueler_isothermal_a_mod

use kinds_mod, only: rp
use param_mod, only: param_type
use state_mod, only: state_type

implicit none
private
public :: climate_bueler_isothermal_a, ice_soln_bueler_isothermal_a


  ! ---------------------------------------------------------------------------
  ! Global parameters (set once, then constant)
  ! ---------------------------------------------------------------------------
  logical, save :: set ! flag indicating if param have been set
  real(rp), save :: M0 ! constant positive surface ice flux [m_ice/a]
  real(rp), save :: L ! fixed ice cap radius [m]
  real(rp), save :: A ! isothermal ice deformation parameter [Pa-3 a-1] 
  real(rp), save :: Mn ! arbitrarily large negative iceflux outside icecap 
  real(rp), allocatable, save :: r(:,:) ! distance from corner [m]


contains


  ! ---------------------------------------------------------------------------
  ! SUB: Set global parameters, executed once
  ! ---------------------------------------------------------------------------
  subroutine init_bueler_isothermal_a(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s

    integer :: i, j

    ! define global parameters
    set = .true.
    M0 = p%ice_soln_param(1)
    L = p%ice_soln_param(2)
    A = p%ice_soln_param(3)
    Mn = -1000.
    allocate(r(p%nx, p%ny))
    do j = 1, p%ny
      do i = 1, p%nx
        r(i,j) = sqrt(s%x(i)*s%x(i)+s%y(j)*s%y(j))
      end do
    end do

    ! check that parameters are sane

    ! both climate_name and ice_soln_name must be bueler_isothermal_a
    if((trim(p%climate_name) .ne. 'bueler_isothermal_a') .or. &
       (trim(p%ice_soln_name) .ne. 'bueler_isothermal_a')) then
       print *, 'Invalid parameters: method requires both climate_name and &
         &ice_soln_name be set to "bueler_isothermal_a"'
       stop 'Stopped.'
    end if 

    ! M0 and L param in climate_param and ice_soln_param must be equal
    if ((p%climate_param(1) .ne. p%ice_soln_param(1)) .or. &
        (p%climate_param(2) .ne. p%ice_soln_param(2))) then
      print *, 'Invalid parameters: method requires that the first 2 elements &
        &of climate_param and ice_soln_param are M0 and L, and must be &
        &equivalent.'
      stop 'Stopped.'
    end if

    ! M0 must be positive
    if (M0 .le. 0.0_rp) then
      print *, 'Invalid parameters: method requires a positive M0'
      stop 'Stopped.'
    end if

    ! L must be positive
    if (L .le. 0.0_rp) then
      print *, 'Invalid parameters: method requires a positive L'
      stop 'Stopped.'
    end if

    ! A must be positive
    if (A .le. 0.0_rp) then
      print *, 'Invalid parameters: method requires a positive A'
      stop 'Stopped.'
    end if

  end subroutine init_bueler_isothermal_a


  ! ---------------------------------------------------------------------------
  ! SUB: Climate update method for this benchmark
  ! ---------------------------------------------------------------------------
  subroutine climate_bueler_isothermal_a(p, s)

    type(param_type), intent(in)    :: p 
    type(state_type), intent(inout) :: s

    if (.not. set) call init_bueler_isothermal_a(p, s)

    where (r .lt. L)
      s%ice_q_surf = M0
    elsewhere
      s%ice_q_surf = Mn
    end where

  end subroutine climate_bueler_isothermal_a


  ! ---------------------------------------------------------------------------
  ! SUB: Ice exact solution method for this benchmark
  ! ---------------------------------------------------------------------------
  subroutine ice_soln_bueler_isothermal_a(p, s)

    type(param_type), intent(in)    :: p 
    type(state_type), intent(inout) :: s

    real(rp) :: c1, c2, gam

    ! initialize parameters, once
    if (.not. set) call init_bueler_isothermal_a(p, s)

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

  end subroutine ice_soln_bueler_isothermal_a


end module test_bueler_isothermal_a_mod
