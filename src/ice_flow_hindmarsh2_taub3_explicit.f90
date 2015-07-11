! =============================================================================
! Ice flow procedures for hindmarsh2_taub3_explicit method
!
! Description: isothermal shallow ice flow flow using Hindmarsh "method 2"
!   stencil, basal sliding using a power law sliding function (sliding_velocity =
!   sliding_coefficient*basal_shear_stress**3), and explicit adaptive timestep.
!   Enabled if ice_name is hindmarsh2_taub3_explicit.
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
!  with units [m/a/Pa**3]. If this variable is not provided in the input file, the
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
! Public: init_hindmarsh2_taub3_explicit, flow_hindmarsh2_taub3_explicit,
!         velo_hindmarsh2_taub3_explicit
! 
! =============================================================================

module ice_flow_hindmarsh3_taub3_explicit

use kinds, only: rp
use param, only: param_type
use state, only: state_type

implicit none
private
public :: init_hindmarsh2_taub3_explicit, flow_hindmarsh2_taub3_explicit, &
         &velo_hindmarsh2_taub3_explicit


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
  ! ABOUT: reusable variables, set in init_hindmarsh2_taub3_explicit
  ! ---------------------------------------------------------------------------


contains


  ! ---------------------------------------------------------------------------
  subroutine init_hindmarsh2_taub3_explicit(p, s)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(in) :: s
  !
  ! ABOUT: check parameters and intializae variables, only once
  ! ---------------------------------------------------------------------------
  
    ! expect exactly 0 parameters
    if (size(p%ice_param) .ne. 0) then
      print *, 'Invalid ice parameters: hindmarsh2_taub3_explicit requires exactly &
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
    allocate(ud(p%nx-1, p%ny-2)); ud = 0.0_rp
    allocate(us(p%nx-1, p%ny-2)); us = 0.0_rp
    allocate(qy(p%nx-2, p%ny-1)); qy = 0.0_rp
    allocate(vd(p%nx-2, p%ny-1)); vd = 0.0_rp
    allocate(vs(p%nx-2, p%ny-1)); vs = 0.0_rp

    ! initialize local constants
    div_dx = 1.0_rp/p%dx 
    div_dy = 1.0_rp/p%dy
    div_4dy = 1.0_rp/(4.0_rp*p%dy)
    div_4dx = 1.0_rp/(4.0_rp*p%dx)
    c_defm = 2.0_rp/5.0_rp*(p%rhoi*p%grav)**3
    c_slid = (p%rhoi*p%grav)**3

  end subroutine init_hindmarsh2_taub3_explicit


  ! ---------------------------------------------------------------------------
  function flow_hindmarsh2_taub3_explicit(p, s) result(dt)
  ! 
    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: compute ice thickness rate-of-change, return stable timestep
  ! ---------------------------------------------------------------------------
    

  end function flow_hindmarsh2_taub3_explicit


  ! ---------------------------------------------------------------------------
  subroutine velo_hindmarsh2_taub3_explicit(p, s)
  ! 
    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: compute ice velocity, first on the staggered grid used above, then
  !   interpolated to the grid points. Only the vertically
  ! ---------------------------------------------------------------------------
    

  end subroutine velo_hindmarsh2_taub3_explicit


end module ice_flow_hindmarsh2_taub3_explicit


