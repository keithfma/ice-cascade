! =============================================================================
! Climate methods for Bueler et al 2005 test D (see ref [1])
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
! Private: 
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
  real(rp), allocatable :: rr(:,:) ! distance from origin [m]
  real(rp), allocatable :: ss(:,:) ! normalized distance from origin, [m]
  real(rp), allocatable :: qs(:,:) ! steady component of surf ice flux, [m/a]
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
    real(rp) :: gam, C, sij

    ! expect exactly 4 parameters
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

    ! A must be positive
    if (A .le. 0.0_rp) then
      print *, 'Invalid ice parameter: hindmarsh2_explicit method requires &
               &a positive A'
      stop 
    end if

    ! allocate grids
    allocate(rr(p%nx, p%ny))
    allocate(ss(p%nx, p%ny))
    allocate(qs(p%nx, p%ny))

    ! compute distance from x = 0, y = 0
    do j = 1, p%ny
      do i = 1, p%nx
        rr(i,j) = sqrt(s%x(i)*s%x(i)+s%y(j)*s%y(j))
        ss(i,j) = rr(i,j)/L
      end do
    end do

    ! compute steady component of surface ice flux
    gam = 2.0_rp/5.0_rp*A*(p%rhoi*p%grav)**3.0_rp 
    C = gam*h0**8.0_rp/(4.0_rp/3.0_rp*L)**3.0_rp
    do j = 1, p%ny
      do i = 1, p%nx

        sij = ss(i,j)
        if (sij .eq. 0.0_rp) then
          qs(i,j) = 2.0_rp*C/L
        elseif (sij .eq. 1.0_rp) then
          qs(i,j) = -C/L
        elseif ((sij .gt. 0.0_rp) .and. (sij .lt. 1.0_rp)) then
          qs(i,j) = C/(L*sij)* &
            (sij**(1.0_rp/3.0_rp)+(1.0_rp-sij)**(1.0_rp/3.0_rp)-1.0_rp)**2.0_rp * &
            (2.0_rp*sij**(1.0_rp/3.0_rp)+(1.0_rp-sij)**(-2.0_rp/3.0_rp)*(1.0_rp-2.0_rp*sij)-1.0_rp)
        else
          qs(i,j) = q0 
        end if

      end do
    end do 

  end subroutine init_bueler_isothermal_d


  ! ---------------------------------------------------------------------------
  subroutine update_bueler_isothermal_d(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: Get climate at current time
  ! ---------------------------------------------------------------------------

  ! DEBUG
  s%ice_q_surf = qs

  end subroutine update_bueler_isothermal_d


end module climate_bueler_isothermal_d

