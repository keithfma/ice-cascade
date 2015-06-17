! =============================================================================
! Ice flow procedures for hindmarsh2_explicit method
!
! Description: isothermal shallow ice flow flow using Hindmarsh "method 2"
!   stencil, no basal sliding and explicit adaptive timestep. Enabled if
!   ice_name is hindmarsh2_explicit

! 
! Spatial discretization: Hindmarsh "method 2" stencil (see [1] and references
!   therein), also commonly refered to as the Mahaffy method (see [2]). Ice flux
!   is computed on at midpoints staggered in the flow direction only.
!
! Time discretization: Explicit time stepping with an adaptive timestep taken
!   (see eq 103 in [3]) 
!
! Parameters:
!   (1) A: isothermal ice deformation parameter [Pa-3 a-1] 
!
! References:
! [1] Hindmarsh, R. C. A., & Payne, A. (1996). Time-step limits for stable
!   solutions of the ice-sheet equation. Annals of Glaciology, 17(4), 391-412.
!   doi:10.1177/030913339301700401
! [2] Mahaffy, M. W. (1976). A Three-Dimensional Numerical Model of Ice Sheets:
!   Tests on the Barnes Ice Cap, Northwest Territories. Journal of Geophysical
!   Research, 81(6), 1059-1066. doi:10.1029/JC081i006p01059
! [3] Hindmarsh, R. C. A. (2001). Notes on basic glaciological computational
!   methods and algorithms. In B. Straughan, R. Greve, H. Ehrentraut, & Y. Wang
!   (Eds.), Continuum Mechanics and Applications in Geophysics and the
!   Environment (pp.  222–249). Springer Berlin Heidelberg.
!   doi:10.1007/978-3-662-04439-1_13
!
! Public: init_hindmarsh2_explicit, flow_hindmarsh2_explicit
! 
! =============================================================================

module ice_flow_hindmarsh2_explicit

use kinds, only: rp
use param, only: param_type
use state, only: state_type

implicit none
private
public :: init_hindmarsh2_explicit, flow_hindmarsh2_explicit


  ! ---------------------------------------------------------------------------
  real(rp) :: A
  real(rp) :: gam
  real(rp), allocatable :: qx(:,:), qy(:,:)
  !
  ! ABOUT: reusable variables, set in init_hindmarsh2_explicit
  ! ---------------------------------------------------------------------------


contains


  ! ---------------------------------------------------------------------------
  subroutine init_hindmarsh2_explicit(p, s)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s
  !
  ! ABOUT: check parameters and intializae variables, only once
  ! ---------------------------------------------------------------------------

    ! expect exactly 1 parameter
    if (size(p%ice_param) .ne. 1) then
      print *, 'Invalid ice parameters: hindmarsh2_explicit requires exactly &
               &1 parameter.'
      stop
    end if

    ! rename parameters
    A = p%ice_param(1)

    ! A must be positive
    if (A .le. 0.0_rp) then
      print *, 'Invalid ice parameter: hindmarsh2_explicit method requires &
               &a positive A'
      stop 
    end if

    ! init other persistent vars
    gam = 2.0_rp/5.0_rp*A*(p%rhoi*p%grav)**3
    allocate(qx(p%nx-1, p%ny-2)); qx = 0.0_rp
    allocate(qy(p%nx-2, p%ny-1)); qy = 0.0_rp

  end subroutine init_hindmarsh2_explicit


  ! ---------------------------------------------------------------------------
  function flow_hindmarsh2_explicit(p, s) result(dt)
  ! 
    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: compute ice thickness rate-of-change, return stable timestep
  ! ---------------------------------------------------------------------------

    integer :: i, j
    real(rp) :: c1, c2, D, Dmax, dsurf_dx_mid, dsurf_dy_mid, dt, ice_h_mid

    Dmax = 0.0_rp

    ! x-direction diffusitivy and ice-flux at midpoints
    c1 = 1.0_rp/p%dx
    c2 = 1.0_rp/(4.0_rp*p%dy)
    do j = 2, p%ny-1
      do i = 1, p%nx-1
        ice_h_mid= 0.5_rp*(s%ice_h(i,j)+s%ice_h(i+1,j))
        dsurf_dx_mid = c1*(s%surf(i+1,j)-s%surf(i,j)) 
        dsurf_dy_mid = c2*(s%surf(i,j+1)-s%surf(i,j-1)+ &
                           s%surf(i+1,j+1)-s%surf(i+1,j-1))
        D = gam*(ice_h_mid**5)*(dsurf_dx_mid*dsurf_dx_mid+ &
                                dsurf_dy_mid*dsurf_dy_mid)
        qx(i,j-1) = -D*dsurf_dx_mid
        Dmax = max(Dmax, D)
      end do
    end do

    ! y-direction diffusitivy and ice-flux at midpoints
    c1 = 1.0_rp/p%dy
    c2 = 1.0_rp/(4.0_rp*p%dx)
    do j = 1, p%ny-1 
      do i = 2, p%nx-1
        ice_h_mid= 0.5_rp*(s%ice_h(i,j)+s%ice_h(i,j+1))
        dsurf_dy_mid = c1*(s%surf(i,j+1)-s%surf(i,j))
        dsurf_dx_mid = c2*(s%surf(i+1,j)-s%surf(i-1,j)+ &
                           s%surf(i+1,j+1)-s%surf(i-1,j+1))
        D = gam*(ice_h_mid**5)*(dsurf_dx_mid*dsurf_dx_mid+ &
                                dsurf_dy_mid*dsurf_dy_mid)
        qy(i-1,j) = -D*dsurf_dy_mid
        Dmax = max(Dmax, D)
      end do
    end do

      ! thickness rate of change
      c1 = -1.0_rp/p%dx
      c2 = -1.0_rp/p%dy
      do j = 2, p%ny-1
        do i = 2, p%nx-1
          s%ice_h_dot(i,j) = c1*(qx(i,j-1)-qx(i-1,j-1))+ &
                             c2*(qy(i-1,j)-qy(i-1,j-1))
        end do
      end do

    dt = p%dx*p%dy/(8.0_rp*Dmax) ! stable

  end function flow_hindmarsh2_explicit


end module ice_flow_hindmarsh2_explicit


