! =============================================================================
! Boundary condition for the glacier dynamics model component of ice-cascade
!
! Description: Points at model boundaries (and ghost points beyond model
!   boundaries) are ice-free. With these set there is no need to specify
!   boundary topography, ice deformation coefficients, or sliding coefficients.
!   Selected for 'ice_bc_name__nesw' parameters that are set to 'no_ice'.
!   Subroutines conform to the template defined in ice.f90
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
    ! s%topo at boundary not used, ghost points not set
    s%ice_h(:,n-1:n) = 0.0_rp
    ! s%ice_a_defm at boundary not used, ghost points not set
    ! s%ice_a_slid at boundary not used, ghost points not set

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
    ! s%topo at boundary not used, ghost points not set
    s%ice_h(n-1:n,:) = 0.0_rp
    ! s%ice_a_defm at boundary not used, ghost points not set
    ! s%ice_a_slid at boundary not used, ghost points not set

  end subroutine ebc_no_ice


  ! ---------------------------------------------------------------------------
  subroutine sbc_no_ice(s)
  !
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: apply no_ice BC to south edge, (:,1)
  ! ---------------------------------------------------------------------------

    ! s%topo at boundary not used, ghost points not set
    s%ice_h(:,1:2) = 0.0_rp
    ! s%ice_a_defm at boundary not used, ghost points not set
    ! s%ice_a_slid at boundary not used, ghost points not set

  end subroutine sbc_no_ice
  

  ! ---------------------------------------------------------------------------
  subroutine wbc_no_ice(s)
  !
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: apply no_ice BC to west edge, (1,:)
  ! ---------------------------------------------------------------------------

    ! s%topo at boundary not used, ghost points not set
    s%ice_h(1:2,:) = 0.0_rp
    ! s%ice_a_defm at boundary not used, ghost points not set
    ! s%ice_a_slid at boundary not used, ghost points not set

  end subroutine wbc_no_ice


end module ice_bc_no_ice
