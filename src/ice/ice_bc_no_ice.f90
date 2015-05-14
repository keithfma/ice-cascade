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
!     test_ice_bc_no_ice 
!
!   Private: none
!
! =============================================================================

module ice_bc_no_ice

use kinds, only: rp
use state, only: state_type

implicit none
private
public :: nbc_no_ice, ebc_no_ice, sbc_no_ice, wbc_no_ice, test_ice_bc_no_ice 


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


! TESTS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  ! ---------------------------------------------------------------------------
  ! FUNC: test all subroutines
  ! ---------------------------------------------------------------------------
  function test_ice_bc_no_ice() result(pass)
    
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
    call nbc_no_ice(s)
    if (any(s%topo(:,ny) .ne. s%topo(:,ny-1))) then
      pass = .false.
      print *, 'North BC topography fails'
    end if
    if (any(s%ice_h(:,ny) .ne. 0.0_rp)) then
      pass = .false.
      print *, 'North BC ice thickness fails'
    end if

    ! test east bc
    call random_number(s%topo)
    call random_number(s%ice_h)
    call ebc_no_ice(s)
    if (any(s%topo(nx,:) .ne. s%topo(nx-1,:))) then
      pass = .false.
      print *, 'East BC topography fails'
    end if
    if (any(s%ice_h(nx,:) .ne. 0.0_rp)) then
      pass = .false.
      print *, 'East BC ice thickness fails'
    end if

    ! test south bc
    call random_number(s%topo)
    call random_number(s%ice_h)
    call sbc_no_ice(s)
    if (any(s%topo(:,1) .ne. s%topo(:,2))) then
      pass = .false.
      print *, 'South BC topography fails'
    end if
    if (any(s%ice_h(:,1) .ne. 0.0_rp)) then
      pass = .false.
      print *, 'South BC ice thickness fails'
    end if

    ! test west bc
    call random_number(s%topo)
    call random_number(s%ice_h)
    call wbc_no_ice(s)
    if (any(s%topo(1,:) .ne. s%topo(2,:))) then
      pass = .false.
      print *, 'West BC topography fails'
    end if
    if (any(s%ice_h(1,:) .ne. 0.0_rp)) then
      pass = .false.
      print *, 'West BC ice thickness fails'
    end if

  end function test_ice_bc_no_ice
  


end module ice_bc_no_ice
