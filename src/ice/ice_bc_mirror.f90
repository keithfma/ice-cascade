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
!     test_ice_bc_mirror 
!
!   Private: none
!
! =============================================================================

module ice_bc_mirror

use kinds, only: rp
use state, only: state_type

implicit none
private
public :: nbc_mirror, ebc_mirror, sbc_mirror, wbc_mirror, test_ice_bc_mirror 


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


! TESTS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  ! ---------------------------------------------------------------------------
  ! FUNC: test all subroutines
  ! ---------------------------------------------------------------------------
  function test_ice_bc_mirror() result(pass)
    
    integer, parameter :: nx = 9 
    integer, parameter :: ny = 11
    
    type(state_type) :: s
    logical :: pass

    ! allocate nx*ny 
    call s%init(nx,ny)

    pass = .true.

    ! test north bc
    call random_number(s%topo)
    call random_number(s%ice_h)
    call nbc_mirror(s)
    if (any(s%topo(:,ny) .ne. s%topo(:,ny-2))) then
      pass = .false.
      print *, 'North BC topography fails'
    end if
    if (any(s%ice_h(:,ny) .ne. s%ice_h(:,ny-2))) then
      pass = .false.
      print *, 'North BC ice thickness fails'
    end if

    ! test east bc
    call random_number(s%topo)
    call random_number(s%ice_h)
    call ebc_mirror(s)
    if (any(s%topo(nx,:) .ne. s%topo(nx-2,:))) then
      pass = .false.
      print *, 'East BC topography fails'
    end if
    if (any(s%ice_h(nx,:) .ne. s%ice_h(nx-2,:))) then
      pass = .false.
      print *, 'East BC ice thickness fails'
    end if

    ! test south bc
    call random_number(s%topo)
    call random_number(s%ice_h)
    call sbc_mirror(s)
    if (any(s%topo(:,1) .ne. s%topo(:,3))) then
      pass = .false.
      print *, 'South BC topography fails'
    end if
    if (any(s%ice_h(:,1) .ne. s%ice_h(:,3))) then
      pass = .false.
      print *, 'South BC ice thickness fails'
    end if

    ! test west bc
    call random_number(s%topo)
    call random_number(s%ice_h)
    call wbc_mirror(s)
    if (any(s%topo(1,:) .ne. s%topo(3,:))) then
      pass = .false.
      print *, 'West BC topography fails'
    end if
    if (any(s%ice_h(1,:) .ne. s%ice_h(3,:))) then
      pass = .false.
      print *, 'West BC ice thickness fails'
    end if

  end function test_ice_bc_mirror

end module ice_bc_mirror
