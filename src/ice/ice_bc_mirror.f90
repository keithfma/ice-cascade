! =============================================================================
! Boundary condition for the glacier dynamics model component of ice-cascade
!
! Description: Ghost points at model boundaries are mirror images across the
! domain edge points for both ice thickness and topography (and by consequence,
! ice flux as well). Selected for 'ice_bc_name__nesw' parameters that are set to
! 'mirror'. Subroutines conform to the template defined in ice.f90
!
!   Public: 
!     nbc_mirror 
!     ebc_mirror 
!     sbc_mirror
!     wbc_mirror
!
!   Private: none
!
! =============================================================================

module ice_bc_mirror

use kinds, only: rp
use state, only: state_type

implicit none
private
public :: nbc_mirror, ebc_mirror, sbc_mirror, wbc_mirror


contains


  ! ---------------------------------------------------------------------------
  ! SUB: apply mirror BC to north edge, (:,end)
  ! ---------------------------------------------------------------------------
  subroutine nbc_mirror(s)

    type(state_type), intent(inout) :: s
    integer :: n

    n = size(s%topo, 2)
    s%topo(:, n) = s%topo(:, n-2)
    s%ice_h(:, n) = s%ice_h(:, n-2)

  end subroutine nbc_mirror
  

  ! ---------------------------------------------------------------------------
  ! SUB: apply mirror BC to east edge, (end,:)
  ! ---------------------------------------------------------------------------
  subroutine ebc_mirror(s)

    type(state_type), intent(inout) :: s
    integer :: n

    n = size(s%topo, 1)
    s%topo(n,:) = s%topo(n-2,:)
    s%ice_h(n,:) = s%ice_h(n-2,:)

  end subroutine ebc_mirror


  ! ---------------------------------------------------------------------------
  ! SUB: apply mirror BC to south edge, (:,1)
  ! ---------------------------------------------------------------------------
  subroutine sbc_mirror(s)

    type(state_type), intent(inout) :: s

    s%topo(:,1) = s%topo(:,3)
    s%ice_h(:,1) = s%ice_h(:,3)

  end subroutine sbc_mirror
  

  ! ---------------------------------------------------------------------------
  ! SUB: apply mirror BC to west edge, (1,:)
  ! ---------------------------------------------------------------------------
  subroutine wbc_mirror(s)

    type(state_type), intent(inout) :: s

    s%topo(1,:) = s%topo(3,:)
    s%ice_h(1,:) = s%ice_h(3,:)

  end subroutine wbc_mirror


end module ice_bc_mirror
