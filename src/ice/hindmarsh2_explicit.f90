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
! Private: A
! 
! =============================================================================

module ice_hindmarsh2_explicit

use kinds, only: rp
use param, only: param_type
use state, only: state_type

implicit none
private
public :: init_hindmarsh2_explicit, flow_hindmarsh2_explicit


  ! ---------------------------------------------------------------------------
  ! VARS: set in init_hindmarsh2_explicit
  ! ---------------------------------------------------------------------------
  real(rp) :: A


contains


  ! ---------------------------------------------------------------------------
  ! SUB: check parameters and intializae variables, only once
  ! ---------------------------------------------------------------------------
  subroutine init_hindmarsh2_explicit(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s

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

  end subroutine init_hindmarsh2_explicit


  ! ---------------------------------------------------------------------------
  ! FUNC: compute ice thickness rate-of-change, return stable timestep
  ! ---------------------------------------------------------------------------
  function flow_hindmarsh2_explicit(p, s) result(dt)
    
    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
    real(rp) :: dt

    dt = 1.0_rp ! DEBUG

  end function flow_hindmarsh2_explicit


end module ice_hindmarsh2_explicit


!
!    ! saved vars (init once)
!    logical, save :: init 
!    real(rp), save :: A, dxinv, dyinv
!    real(rp), allocatable, save :: qx(:,:), qy(:,:), thck(:,:), surf(:,:) 
!
!    ! unsaved vars
!    integer :: i, ie, iw, j, jn, js 
!    real(rp) :: c1, c2, dif, dif_max, dt, dsurf_dx_mid, dsurf_dy_mid, gam, t, thck_mid
!    
!    ! init, first time only
!    if (.not. init) then
!      A = p%ice_param(1)
!      allocate(thck(p%nx+2, p%ny+2), surf(p%nx+2, p%ny+2), &
!               qx(p%nx+1, p%ny), qy(p%nx, p%ny+1))
!      thck = 0.0_rp; surf = 0.0_rp; qx = 0.0_rp; qy = 0.0_rp
!      init = .true.
!    end if
!
!    ! constants
!    js = 2; jn = p%ny+1
!    iw = 2; ie = p%nx+1
!    gam = 2.0_rp/5.0_rp*A*(p%rhoi*p%grav)**3
!
!    ! copy in shared values
!    thck(iw:ie, js:jn) = s%ice_h
!    surf(iw:ie, js:jn) = s%ice_h+s%topo
!
!    ! srt time stepping
!    t = 0.0_rp
!    dt = p%time_step ! DEBUG ONLY
!    do while (t .lt. p%time_step)
!
!      ! boundary conditions
!      call g%apply_nbc(surf(:,jn), surf(:,jn-1), surf(:,js), surf(:,jn+1), &
!                       thck(:,jn), thck(:,jn-1), thck(:,js), thck(:,jn+1))
!      call g%apply_sbc(surf(:,js), surf(:,js+1), surf(:,jn), surf(:,js-1), &
!                       thck(:,js), thck(:,js+1), thck(:,jn), thck(:,js-1))
!      call g%apply_ebc(surf(ie,:), surf(ie-1,:), surf(iw,:), surf(ie+1,:), &
!                       thck(ie,:), thck(ie-1,:), thck(iw,:), thck(ie+1,:))
!      call g%apply_wbc(surf(iw,:), surf(iw+1,:), surf(ie,:), surf(iw-1,:), &
!                       thck(iw,:), thck(iw+1,:), thck(ie,:), thck(iw-1,:))
!
!      ! ice flux
!      ! x-direction
!      dif_max = 0.0_rp
!      c1 = 1.0_rp/p%dx
!      c2 = 1.0_rp/(4.0_rp*p%dy)
!      do j = js, jn
!        do i = 1, ie
!          thck_mid = 0.5_rp*(thck(i,j)+thck(i+1,j))
!          dsurf_dx_mid = c1*(surf(i+1,j)-surf(i,j)) 
!          dsurf_dy_mid = c2*(surf(i,j+1)-surf(i,j-1)+surf(i+1,j+1)-surf(i+1,j-1))
!          dif = gam*(thck_mid**5)*(dsurf_dx_mid**2+dsurf_dy_mid**2)
!          qx(i,j-1) = -dif*dsurf_dx_mid
!          dif_max = max(dif_max, dif)
!        end do
!      end do
!      ! y-direction
!      c1 = 1.0_rp/p%dy
!      c2 = 1.0_rp/(4.0_rp*p%dx)
!      do j = 1, jn
!        do i = iw, ie
!          thck_mid = 0.5_rp*(thck(i,j)+thck(i,j+1))
!          dsurf_dy_mid = c1*(surf(i,j+1)-surf(i,j))
!          dsurf_dx_mid = c2*(surf(i+1,j)-surf(i-1,j)+surf(i+1,j+1)-surf(i-1,j+1))
!          dif = gam*(thck_mid**5)*(dsurf_dx_mid**2+dsurf_dy_mid**2)
!          qy(i-1,j) = -dif*dsurf_dy_mid
!          dif_max = max(dif_max, dif)
!        end do
!      end do
!
!      ! thickness rate of change
!      c1 = 1.0_rp/p%dx
!      c2 = 1.0_rp/p%dy
!      do j = 1, p%ny
!        do i = 1, p%nx
!          s%ice_h_dot(i,j) = c1*(qx(i+1,j)-qx(i,j))+c2*(qy(i,j+1)-qy(i,j))
!        end do
!      end do
!
!      ! timestep
!      dt = p%dx*p%dy/(8.0_dp*dif_max) ! stable
!      dt = min(dt, p%time_step-t) ! trimmed
!
!      ! update thickness
!      thck(iw:ie, js:jn) = thck(iw:ie, js:jn)+dt*(s%ice_h_dot+s%ice_q_surf)
!      thck = max(thck, 0.0_rp)
!      surf(iw:ie, js:jn) = thck(iw:ie, js:jn)+s%topo
!      
!      ! increment time
!      t = t+dt
!   
!    end do
!    ! end time stepping 
!
!    ! velocities
!
!    ! copy out shared variables
!    s%ice_h = thck(iw:ie, js:jn)
!
!  end subroutine update_hindmarsh2_explicit
