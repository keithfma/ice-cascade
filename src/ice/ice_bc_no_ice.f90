! =============================================================================
! Boundary condition for the glacier dynamics model component of ice-cascade
!
! Description: Ghost points at model boundaries are ice-free, topography is
!   flat. Selected for 'ice_bc_name__nesw' parameters that are set to 'no_ice'.
!   Subroutines conform to the template defined in ice.f90
!
!   Public: 
!     nbc_no_ice 
!     ebc_no_ice 
!     sbc_no_ice
!     wbc_no_ice
!
!   Private: none
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
  ! SUB: apply no_ice BC to north edge, (:,end)
  ! ---------------------------------------------------------------------------
  subroutine nbc_no_ice(s)

    type(state_type), intent(inout) :: s
    integer :: n

    n = size(s%topo, 2)
    s%topo(:, n) = s%topo(:, n-1)
    s%ice_h(:, n) = 0.0_rp

  end subroutine nbc_no_ice
  

  ! ---------------------------------------------------------------------------
  ! SUB: apply no_ice BC to east edge, (end,:)
  ! ---------------------------------------------------------------------------
  subroutine ebc_no_ice(s)

    type(state_type), intent(inout) :: s
    integer :: n

    n = size(s%topo, 1)
    s%topo(n,:) = s%topo(n-1,:)
    s%ice_h(n,:) = 0.0_rp

  end subroutine ebc_no_ice


  ! ---------------------------------------------------------------------------
  ! SUB: apply no_ice BC to south edge, (:,1)
  ! ---------------------------------------------------------------------------
  subroutine sbc_no_ice(s)

    type(state_type), intent(inout) :: s

    s%topo(:,1) = s%topo(:,2)
    s%ice_h(:,1) = 0.0_rp

  end subroutine sbc_no_ice
  

  ! ---------------------------------------------------------------------------
  ! SUB: apply no_ice BC to west edge, (1,:)
  ! ---------------------------------------------------------------------------
  subroutine wbc_no_ice(s)

    type(state_type), intent(inout) :: s

    s%topo(1,:) = s%topo(2,:)
    s%ice_h(1,:) = 0.0_rp

  end subroutine wbc_no_ice


end module ice_bc_no_ice
