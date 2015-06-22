! =============================================================================
! Boundary condition for the glacier dynamics model component of ice-cascade
!
! Description: No ice flux across model boundary. This is accomplished by
!   forcing the surface gradient at boundary points to zero. The ghost point
!   topography and ice thickness are set equal to the adjacent interior point.
!   The deformation and sliding coefficients are irrelevant for these points
!   because the gradient goes to zero the flux term goes to xero regardless of
!   the coefficient value - they are not set for this reason. Selected for
!   'ice_bc_name__nesw' parameters that are set to 'no_flux'.  Subroutines
!   conform to the template defined in ice.f90
!
!   Public: nbc_no_flux, ebc_no_flux, sbc_no_flux, wbc_no_flux
!
! =============================================================================

module ice_bc_no_flux

use kinds, only: rp
use state, only: state_type

implicit none
private
public :: nbc_no_flux, ebc_no_flux, sbc_no_flux, wbc_no_flux


contains


  ! ---------------------------------------------------------------------------
  subroutine nbc_no_flux(s)
  !
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: apply no_flux BC to north edge, (:,end)
  ! ---------------------------------------------------------------------------
  
    integer :: n

    n = size(s%topo, 2)
    s%topo(:, n) = s%topo(:, n-1)
    s%ice_h(:, n) = s%ice_h(:, n-1)
    ! s%ice_a_defm not used, so ghost points are not set 
    ! s%ice_a_slid not used, so ghost points are not set 

  end subroutine nbc_no_flux
  

  ! ---------------------------------------------------------------------------
  subroutine ebc_no_flux(s)
  !
    type(state_type), intent(inout) :: s
  ! 
  ! ABOUT: apply no_flux BC to east edge, (end,:)
  ! ---------------------------------------------------------------------------

    integer :: n

    n = size(s%topo, 1)
    s%topo(n,:) = s%topo(n-1,:)
    s%ice_h(n,:) = s%ice_h(n-1,:)
    ! s%ice_a_defm not used, so ghost points are not set 
    ! s%ice_a_slid not used, so ghost points are not set 

  end subroutine ebc_no_flux


  ! ---------------------------------------------------------------------------
  subroutine sbc_no_flux(s)
  !
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: apply no_flux BC to south edge, (:,1)
  ! ---------------------------------------------------------------------------

    s%topo(:,1) = s%topo(:,2)
    s%ice_h(:,1) = s%ice_h(:,2)
    ! s%ice_a_defm not used, so ghost points are not set 
    ! s%ice_a_slid not used, so ghost points are not set 

  end subroutine sbc_no_flux
  

  ! ---------------------------------------------------------------------------
  subroutine wbc_no_flux(s)
  !
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: apply no_flux BC to west edge, (1,:)
  ! ---------------------------------------------------------------------------

    s%topo(1,:) = s%topo(2,:)
    s%ice_h(1,:) = s%ice_h(2,:)
    ! s%ice_a_defm not used, so ghost points are not set 
    ! s%ice_a_slid not used, so ghost points are not set 

  end subroutine wbc_no_flux


end module ice_bc_no_flux
