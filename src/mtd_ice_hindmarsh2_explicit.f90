! =============================================================================
! Ice flow method: Isothermal shallow ice flow flow using Glen's flow law with
!   exponent 3, no basal sliding, Hindmarsh "method 2" stencil, explicit
!   adaptive timestep.
!
! Enabled if the input parameter 'ice_name' is 'hindmarsh_m2_explicit'
!
! Spatial discretization: Hindmarsh "method 2" stencil (see [1] and references
!   therein), also commonly refered to as the Mahaffy method (see [2]). Ice flux
!   is computed on at midpoints staggered in the flow direction only.
!
! Time discretization: Explicit time stepping with an adaptive timestep taken
!   (see eq 103 in [3]) 
!
! Contains:
!   NOTE: list is not yet complete
!
! References:
!
! [1] Hindmarsh, R. C. A., & Payne, A. (1996). Time-step limits for stable
!   solutions of the ice-sheet equation. Annals of Glaciology, 17(4), 391-412.
!   doi:10.1177/030913339301700401
!
! [2] Mahaffy, M. W. (1976). A Three-Dimensional Numerical Model of Ice Sheets:
!   Tests on the Barnes Ice Cap, Northwest Territories. Journal of Geophysical
!   Research, 81(6), 1059-1066. doi:10.1029/JC081i006p01059
!
! [3] Hindmarsh, R. C. A. (2001). Notes on basic glaciological computational
!   methods and algorithms. In B. Straughan, R. Greve, H. Ehrentraut, & Y. Wang
!   (Eds.), Continuum Mechanics and Applications in Geophysics and the
!   Environment (pp.  222–249). Springer Berlin Heidelberg.
!   doi:10.1007/978-3-662-04439-1_13
!
! ============================================================================

module mtd_ice_hindmarsh2_explicit_mod

use kinds_mod, only: rp
use param_mod, only: param_type
use state_mod, only: state_type

implicit none
private
public :: ice_update_hindmarsh2_explicit


  ! ---------------------------------------------------------------------------
  ! Global parameters (set once, then constant)
  ! ---------------------------------------------------------------------------
  logical, save :: set ! flag indicating if param have been set
  real(rp), save :: A ! isothermal ice deformation parameter [Pa-3 a-1] 


contains


  ! ---------------------------------------------------------------------------
  ! SUB: Set global parameters, executed once
  ! ---------------------------------------------------------------------------
  subroutine init_hindmarsh2_explicit(p)

    type(param_type), intent(in) :: p

    set = .true.
    A = p%ice_param(1)

  end subroutine init_hindmarsh2_explicit


  ! ---------------------------------------------------------------------------
  ! SUB: Ice flow model
  ! ---------------------------------------------------------------------------
  subroutine ice_update_hindmarsh2_explicit(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s

    if (.not. set) call init_hindmarsh2_explicit(p)

    ! NOT YET IMPLEMENTED

  end subroutine ice_update_hindmarsh2_explicit


end module mtd_ice_hindmarsh2_explicit_mod
