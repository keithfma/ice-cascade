module types
! Common variable type definitions used by the ice-cascade model
! 
! Note: Older compilers do not support the Fortran 2008 standard
!   iso_fortran_env module. For this reason, older methods are currently used to
!   define floating point precision.

! use, intrinsic :: iso_fortran_env, only: real64, real32 ! Fortran >= 2008
use netcdf, only: nf90_float, nf90_double, nf90_int
use mpi, only: mpi_double_precision, mpi_real

implicit none

save
private :: dummy_dp, dummy_sp

!integer, parameter :: dp = real64 ! Fortran >= 2008 
!integer, parameter :: sp = real32 ! Fortran >= 2008
integer, parameter :: dp = kind(1.d0) ! Fortran < 2008
integer, parameter :: sp = kind(1.0) ! Fortran < 2008 
integer, parameter :: dp_mpi = mpi_double_precision
integer, parameter :: sp_mpi = mpi_real
integer, parameter :: dp_nc = nf90_double
integer, parameter :: sp_nc = nf90_float
integer, parameter :: int_nc = nf90_int

real(dp) :: dummy_dp
real(sp) :: dummy_sp
real(dp), parameter :: dp_eps = epsilon(dummy_dp)
real(dp), parameter :: sp_eps = epsilon(dummy_sp)

end module types
