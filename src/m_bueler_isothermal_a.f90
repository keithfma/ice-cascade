! =============================================================================
! Methods for the bueler_isothermal_a test case.
!
! The methods will be invoked in the case the parameter climate_name  and
! ice_soln_name are both is set to 'bueler_isothermal_a'. 
!
! Contains:
!
!   init_bueler_isothermal_a: Checks for sane parameters and sets global
!   variables. Invoked once by either climate or ice init() routines.!
!
!   climate_bueler_isothermal_a: Invoked if climate_name is set to
!     'bueler_isothermal_a'. The surface ice flux is set to a constant (M0
!     defined in climate_param(1)) within a fixed radius from the ice cap center
!     (L, defined in climate_param(2)), and to a large negative number outside
!     of this radius (arbitrarily large).
!  
!   ice_soln_bueler_isothermal_a: Invoked if ice_soln_name is set to
!   'bueler_isothermal_a'. The solution is described in reference (1), and
!   requires 3 parameters: M0, the constant positive balance rate within th ice
!   cap, L, the constant fixed radius of the ice cap, and A, the constant ice deformation
!   flow factor. These 3 parameters are elements 1, 2, 3 in ice_soln_param,
!   respectively. 

! The 
!   NOTE: list is not yet complete
!
! References:
! (1) Bueler et al 2005...
!
! ============================================================================

module m_bueler_isothermal_a_mod

use kinds_mod, only: rp
use param_mod, only: param_type
use state_mod, only: state_type

implicit none
private
!public ::


  ! ---------------------------------------------------------------------------
  ! Global parameters (set once, then constant)
  ! ---------------------------------------------------------------------------
  logical, save :: set ! flag indicating if param have been set
  real(rp), save :: M0 ! constant positive surface ice flux [m_ice/a]
  real(rp), save :: L ! fixed ice cap radius [m]
  real(rp), save :: A ! isothermal ice deformation parameter [Pa-3 a-1] 
  real(rp), allocatable, save :: r(:,:) ! distance from corner [m]


contains


  ! ---------------------------------------------------------------------------
  ! SUB: Set global parameters, executed once
  ! ---------------------------------------------------------------------------
  subroutine init_bueler_isothermal_a(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s

    ! check for sane inputs

    ! both climate_name and ice_soln_name must be bueler_isothermal_a
    if((trim(p%climate_name) .ne. 'bueler_isothermal_a') .or. &
       (trim(p%ice_soln_name) .ne. 'bueler_isothermal_a')) then
       print *, 'Invalid parameters: bueler_isothermal_a method requires both&
         &and ice_soln_name be set to "bueler_isothermal_a"'
       stop 'Stopped.'
     end if 

    ! M0 and L param in climate_param and ice_soln_param must be equal

    ! M0 must be positive

    ! L must be positive

    ! define parameters as global variables


  end subroutine init_bueler_isothermal_a

end module m_bueler_isothermal_a_mod

  !! ---------------------------------------------------------------------------
  !! SUB: Benchmark solution, Bueler isothermal A
  !!   Parameters
  !!   L: fixed ice cap radius [m] =  g%param_soln(1)
  !!   M0: constant positive surface ice flux [m_ice/a] =  g%param_soln(2)
  !!   A: isothermal ice deformation parameter [Pa-3 a-1]  = g%A0
  !! ---------------------------------------------------------------------------
  !subroutine solve_bueler_isothermal_a(g, s)

  !  class(ice_type), intent(in) :: g
  !  type(state_type), intent(inout) :: s

  !  integer :: i, j
  !  real(rp) :: A, c1, c2, gam, L, M0, r

  !  ! rename parameters for local use
  !  L = g%param_soln(1)     
  !  M0 = g%param_soln(2)
  !  A = g%A0

  !  ! pre-compute constants
  !  gam = 2.0_rp/5.0_rp*A*(s%rhoi*s%grav)**3.0_rp
  !  c1 = (4.0_rp*M0/gam)**(1.0_rp/8.0_rp)
  !  c2 = L**(4.0_rp/3.0_rp)

  !  ! solve
  !  do j = 1, s%ny+2
  !    do i = 1, s%nx+2
  !      r = sqrt(s%x(i)*s%x(i)+s%y(j)*s%y(j))
  !      if (r .le. L) then
  !        s%ice_h_soln(i,j) = c1*(c2-r**(4.0_rp/3.0_rp))**(3.0_rp/8.0_rp)
  !      else
  !        s%ice_h_soln(i,j) = 0.0_rp
  !      end if
  !    end do
  !  end do

  !end subroutine solve_bueler_isothermal_a
