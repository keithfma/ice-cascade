! =============================================================================
! Ice flow method: Isothermal shallow ice flow flow using Glen's flow law with
!   exponent 3, no basal sliding, Hindmarsh "method 2" stencil, explicit
!   adaptive timestep.
!
! Enabled if the input parameter 'ice_name' is 'hindmarsh2_explicit'
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


contains


  ! ---------------------------------------------------------------------------
  ! SUB: Run ice flow model for the specified time step
  ! ---------------------------------------------------------------------------
  subroutine ice_update_hindmarsh2_explicit(prm, sta)

    type(param_type), intent(in) :: prm
    type(state_type), intent(inout) :: sta

    ! saved vars (init once)
    logical, save :: init ! flag indicating if saved vars have been initialized
    real(rp), save :: A ! isothermal ice deformation parameter [Pa-3 a-1] 
    real(rp), allocatable, save :: h(:,:) ! ice thickness, w/ ghost pts
    real(rp), allocatable, save :: s(:,:) ! ice/bedrock surface elev, w/ ghost pts
    real(rp), allocatable, save :: Dx(:,:) ! diffusivity at x-midpoints 
    real(rp), allocatable, save :: Dy(:,:) ! diffusivity at y-midpoints
    real(rp), allocatable, save :: qx(:,:) ! ice flux at x-midpoints
    real(rp), allocatable, save :: qy(:,:) ! ice flux at y-midpoints

    ! unsaved vars
    real(rp) :: t, dt 
    
    ! init, first time only
    if (.not. init) then
      A = prm%ice_param(1)
      allocate(h(prm%nx+2, prm%ny+2))
      allocate(s(prm%nx+2, prm%ny+2))
      allocate(Dx(prm%nx+1, prm%ny))
      allocate(qx(prm%nx+1, prm%ny))
      allocate(Dy(prm%nx, prm%ny+1))
      allocate(qy(prm%nx, prm%ny+1))
      init = .true.
    end if

    ! copy in shared values
    h(2:prm%nx+1, 2:prm%ny+1) = sta%ice_h
    s(2:prm%nx+1, 2:prm%ny+1) = sta%ice_h+sta%topo
    
    ! start time stepping
    t = 0.0_rp
    do while (t .lt. prm%time_step)

      ! boundary conditions
      call 
  subroutine bc_no_ice(topo_edge, topo_intr, topo_oppo, topo_bnd, &
                       ice_edge, ice_intr, ice_oppo, ice_bnd) 


      ! diffusivity and ice flux

      ! thickness rate of change 
   
    end do
    ! end time stepping 

    ! velocities

    ! copy out shared variables

  end subroutine ice_update_hindmarsh2_explicit


end module mtd_ice_hindmarsh2_explicit_mod
