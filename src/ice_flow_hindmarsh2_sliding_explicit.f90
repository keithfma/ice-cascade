! =============================================================================
! Ice flow procedures for hindmarsh2_sliding_explicit method
!
! Description: isothermal shallow ice flow flow using Hindmarsh "method 2"
!   stencil, basal sliding using a linear sliding relation (sliding_velocity =
!   sliding_coefficient * basal_shear_stress ** power), and explicit adaptive
!   timestep. Enabled if ice_name is hindmarsh2_sliding_explicit
! 
! Spatial discretization: Hindmarsh "method 2" stencil (see [1] and references
!   therein), also commonly refered to as the Mahaffy method (see [2]). Ice flux
!   is computed on at midpoints staggered in the flow direction only.
!
! Time discretization: Explicit time stepping with an adaptive timestep taken
!   (see eq 103 in [3]) 

! NOTE: ice deformation parameter is read from the state variable ice_a_defm
!  with units [1/Pa3/a]. If this variable is not provided in the input file, the
!  default value of 0.0 will be used, and the ice will not deform.
!
! NOTE: ice sliding parameter is read from the state variable ice_a_slid
!  with units [m/a/Pa]. If this variable is not provided in the input file, the
!  default value of 0.0 will be used, and the ice will not slide.
!
! Parameters:
!   None.  
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
! Public: init_hindmarsh2_sliding_explicit, flow_hindmarsh2_sliding_explicit.
!         velo_hindmarsh2_sliding_explicit
! 
! =============================================================================

module ice_flow_hindmarsh2_sliding_explicit

use kinds, only: rp
use param, only: param_type
use state, only: state_type

implicit none
private
public :: init_hindmarsh2_sliding_explicit, flow_hindmarsh2_sliding_explicit, &
         &velo_hindmarsh2_sliding_explicit


  ! ---------------------------------------------------------------------------
  real(rp), allocatable :: qx(:,:), qy(:,:)  ! ice flux, grid at midpts
  real(rp), allocatable :: ud(:,:), vd(:,:) ! ice deformation velocity at midpts
  real(rp), allocatable :: us(:,:), vs(:,:) ! ice sliding velocity at midpts
  real(rp) :: div_dx ! constant factor in computations
  real(rp) :: div_dy ! " " 
  real(rp) :: div_4dx ! " " 
  real(rp) :: div_4dy ! " " 
  real(rp) :: c_defm ! " " 
  real(rp) :: c_slid ! " " 
  !
  ! ABOUT: reusable variables, set in init_hindmarsh2_sliding_explicit
  ! ---------------------------------------------------------------------------


contains


  ! ---------------------------------------------------------------------------
  subroutine init_hindmarsh2_sliding_explicit(p, s)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s
  !
  ! ABOUT: check parameters and intializae variables, only once
  ! ---------------------------------------------------------------------------

    ! expect exactly 0 parameters
    if (size(p%ice_param) .ne. 0) then
      print *, 'Invalid ice parameters: hindmarsh2_sliding_explicit requires exactly &
               &0 parameters.'
      stop
    end if

    ! error if ice deformation/sliding coefficient is anywhere negative
    if (any(s%ice_a_defm .lt. 0.0_rp)) then
      print *, 'ERROR: Ice deformation coefficient must be non-negative'
      stop
    end if

    if (any(s%ice_a_slid .lt. 0.0_rp)) then
      print *, 'ERROR: Ice sliding coefficient must be non-negative'
      stop
    end if

    ! allocate local parameters 
    allocate(qx(p%nx-1, p%ny-2)); qx = 0.0_rp
    allocate(qy(p%nx-2, p%ny-1)); qy = 0.0_rp
    allocate(ud(p%nx-1, p%ny-2)); ud = 0.0_rp
    allocate(vd(p%nx-2, p%ny-1)); vd = 0.0_rp
    allocate(us(p%nx-1, p%ny-2)); us = 0.0_rp
    allocate(vs(p%nx-2, p%ny-1)); vs = 0.0_rp

    ! initialize local constants
    div_dx = 1.0_rp/p%dx 
    div_dy = 1.0_rp/p%dy
    div_4dy = 1.0_rp/(4.0_rp*p%dy)
    div_4dx = 1.0_rp/(4.0_rp*p%dx)
    c_defm = 2.0_rp/5.0_rp*(p%rhoi*p%grav)**3
    c_slid = p%rhoi*p%grav

  end subroutine init_hindmarsh2_sliding_explicit


  ! ---------------------------------------------------------------------------
  function flow_hindmarsh2_sliding_explicit(p, s) result(dt)
  ! 
    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: compute ice thickness rate-of-change, return stable timestep
  ! ---------------------------------------------------------------------------
    
    integer :: i, j 
    real(rp) :: D, Dmax, dsurf_dx_mid, dsurf_dy_mid, dt, a_defm_mid, a_slid_mid, h_mid, surf_grad2

    Dmax = 0.0_rp

    ! x-direction diffusitivy and ice-flux at midpoints
    do j = 2, p%ny-1
      do i = 1, p%nx-1
        h_mid = 0.5_rp*(s%ice_h(i,j)+s%ice_h(i+1,j))
        a_defm_mid = 0.5_rp*(s%ice_a_defm(i,j)+s%ice_a_defm(i+1,j))
        a_slid_mid = 0.5_rp*(s%ice_a_slid(i,j)+s%ice_a_slid(i+1,j))
        dsurf_dx_mid = (s%surf(i+1,j)-s%surf(i,j))*div_dx 
        dsurf_dy_mid = (s%surf(i  ,j+1)-s%surf(i  ,j-1)+ &
                        s%surf(i+1,j+1)-s%surf(i+1,j-1))*div_4dy
        surf_grad2 = dsurf_dx_mid**2+dsurf_dy_mid**2
        D = c_defm*a_defm_mid*h_mid**5*surf_grad2 + c_slid*a_slid_mid*h_mid**2 
        qx(i,j-1) = -D*dsurf_dx_mid
        Dmax = max(Dmax, D)
      end do
    end do

    ! y-direction diffusitivy and ice-flux at midpoints
    do j = 1, p%ny-1 
      do i = 2, p%nx-1
        h_mid= 0.5_rp*(s%ice_h(i,j)+s%ice_h(i,j+1))
        a_defm_mid = 0.5_rp*(s%ice_a_defm(i,j)+s%ice_a_defm(i,j+1))
        a_slid_mid = 0.5_rp*(s%ice_a_slid(i,j)+s%ice_a_slid(i,j+1))
        dsurf_dy_mid = (s%surf(i,j+1)-s%surf(i,j))*div_dy
        dsurf_dx_mid = (s%surf(i+1,j  )-s%surf(i-1,j  )+ &
                        s%surf(i+1,j+1)-s%surf(i-1,j+1))*div_4dx
        surf_grad2 = dsurf_dx_mid**2+dsurf_dy_mid**2
        D = c_defm*a_defm_mid*h_mid**5*surf_grad2 + c_slid*a_slid_mid*h_mid**2 
        qy(i-1,j) = -D*dsurf_dy_mid
        Dmax = max(Dmax, D)
      end do
    end do

    ! thickness rate of change
    do j = 2, p%ny-1
      do i = 2, p%nx-1
        s%ice_h_dot(i,j) = -(qx(i  ,j-1)-qx(i-1,j-1))*div_dx &
                           -(qy(i-1,j  )-qy(i-1,j-1))*div_dy
      end do
    end do

    ! compute time step, dealing with ice-free case  
    if (Dmax .eq. 0.0_rp) then
      dt = s%step 
    else  
      dt = p%dx*p%dy/(8.0_rp*Dmax) 
    end if

  end function flow_hindmarsh2_sliding_explicit


  ! ---------------------------------------------------------------------------
  subroutine velo_hindmarsh2_sliding_explicit(p, s)
  ! 
    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: compute ice velocity, first on the staggered grid used above, then
  !   interpolated to the grid points. Only the vertically
  ! ---------------------------------------------------------------------------


  end subroutine velo_hindmarsh2_sliding_explicit


end module ice_flow_hindmarsh2_sliding_explicit


