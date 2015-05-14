! =============================================================================
! Run unit tests for the ice-cascade model 
!
!   Description: Many module components of ice-cascade include testing routines
!     that verify that they are working as intended. This program runs all of
!     these tests and reports the results.
!
! =============================================================================

program test_ice_cascade

use ice_bc_no_ice, only: test_ice_bc_no_ice
use ice_bc_mirror, only: test_ice_bc_mirror

implicit none

logical :: pass

pass = test_ice_bc_no_ice()
if (pass) then
  print *, 'test_ice_bc_no_ice: PASS'
else
  print *, 'test_ice_bc_no_ice: FAIL'
end if

pass = test_ice_bc_mirror()
if (pass) then
  print *, 'test_ice_bc_mirror: PASS'
else
  print *, 'test_ice_bc_mirror: FAIL'
end if

end program test_ice_cascade
