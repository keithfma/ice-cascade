! =============================================================================
! Boundary condition for the glacier dynamics model component of ice-cascade
!
! Description: Ghost points at model boundaries are ice-free, topography is
!   flat, ice deformation and sliding coefficients are set equal to the adjacent
!   interior points.  Selected for 'ice_bc_name__nesw' parameters that are set
!   to 'no_ice'.  Subroutines conform to the template defined in ice.f90
!
! Public: nbc_no_ice, ebc_no_ice, sbc_no_ice, wbc_no_ice
!
! =============================================================================

module ice_bc_no_ice

use kinds, only: rp
use state, only: state_type

implicit none
private
public :: nbc_no_ice, ebc_no_ice, sbc_no_ice, wbc_no_ice


contains


  ! ---------------------------------------------------------------------------
  subroutine nbc_no_ice(s)
  !
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: apply no_ice BC to north edge, (:,end)
  ! ---------------------------------------------------------------------------

    integer :: n

    n = size(s%topo, 2)
    s%topo(:,n) = s%topo(:, n-1)
    s%ice_h(:,n) = 0.0_rp
    s%ice_a_defm(:,n) = s%ice_a_defm(:,n-1)
    s%ice_a_slid(:,n) = s%ice_a_slid(:,n-1)

  end subroutine nbc_no_ice
  

  ! ---------------------------------------------------------------------------
  subroutine ebc_no_ice(s)
  !
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: apply no_ice BC to east edge, (end,:)
  ! ---------------------------------------------------------------------------

    integer :: n

    n = size(s%topo, 1)
    s%topo(n,:) = s%topo(n-1,:)
    s%ice_h(n,:) = 0.0_rp
    s%ice_a_defm(n,:) = s%ice_a_defm(n-1,:)
    s%ice_a_slid(n,:) = s%ice_a_slid(n-1,:)

  end subroutine ebc_no_ice


  ! ---------------------------------------------------------------------------
  subroutine sbc_no_ice(s)
  !
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: apply no_ice BC to south edge, (:,1)
  ! ---------------------------------------------------------------------------

    s%topo(:,1) = s%topo(:,2)
    s%ice_h(:,1) = 0.0_rp
    s%ice_a_defm(:,1) = s%ice_a_defm(:,2)
    s%ice_a_slid(:,1) = s%ice_a_slid(:,2)

  end subroutine sbc_no_ice
  

  ! ---------------------------------------------------------------------------
  subroutine wbc_no_ice(s)
  !
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: apply no_ice BC to west edge, (1,:)
  ! ---------------------------------------------------------------------------

    s%topo(1,:) = s%topo(2,:)
    s%ice_h(1,:) = 0.0_rp
    s%ice_a_defm(1,:) = s%ice_a_defm(2,:)
    s%ice_a_slid(1,:) = s%ice_a_slid(2,:)

  end subroutine wbc_no_ice


end module ice_bc_no_ice
