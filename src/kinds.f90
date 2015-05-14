! =============================================================================
! Variable kind parameter definitaions. Includes both native Fortran kinds, and
! special parameters for netCDF
!
! Contains
!   (public) rp = selected real precision for Fortran variables
!   (public) rp_nc = selected real precision for netCDF variables 
! =============================================================================
module kinds 

use netcdf, only: nf90_float, nf90_double, nf90_int

implicit none
private 
public :: rp, rp_nc

! Generic types
integer, parameter :: dp = kind(1.d0) ! OK for Fortran < 2008
integer, parameter :: sp = kind(1.0) ! OK for Fortran < 2008 
integer, parameter :: dp_nc = nf90_double
integer, parameter :: sp_nc = nf90_float
integer, parameter :: int_nc = nf90_int

! Selected types
integer, parameter :: rp = dp 
integer, parameter :: rp_nc = dp_nc

end module kinds
