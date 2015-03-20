module flexure

! Description:
!! Compute flexural isostatic deflection in response to a combined load of ice and crustal rock 
!! by filtering in the wave domain. Computations are as described by Watts, 2001 (Isostasy and 
!! Flexure of the Lithosphere, p. 177, eq. 5.2). The transforms are computed using the FFTW3 
!! library, and the input grids are padded out to a specified size using the "expand" taper given 
!! by Blakley, 1996 (Potential Theory in Gravity and Magnetic Applications, Appendix B). 

! Module contains:
!! flexInit: allocate flexure variables and generate the FFTW plans
!! flex: compute flexural response to a combined ice & crustal rock load

! Usage:
!! (1) Call "flexInit" once to initialise the FFTW transforms and allocate the working arrays. So 
!! long as the array sizes/types do not change, there is no need to call this routine again.
!! (2) Call "flex" for each computation of the flexural deflection. Note that the final relaxed 
!! topography is (final topography) = (initial topo)+(deflection).

use types, only: dp
use io, only: debugOut2d
use, intrinsic :: iso_c_binding
use fftw3, only: fftw_plan_dft_r2c_2d, fftw_plan_dft_c2r_2d, fftw_execute_dft_r2c, &
				fftw_execute_dft_c2r, fftw_destroy_plan, FFTW_ESTIMATE, FFTW_PATIENT, &
				FFTW_MEASURE, FFTW_EXHAUSTIVE
public :: flexInit, flex
private

! Common variables, saved 
real(C_DOUBLE), allocatable :: sLoadPad(:,:)
complex(C_DOUBLE_COMPLEX), allocatable :: wLoadPad(:,:)
type(C_PTR) :: planFwd, planInv
save :: sLoadPad, wLoadPad, planFwd, planInv
				
contains

! ==================================================================================================
! flexInit: allocate flexure variables and generate the FFTW plans
! ==================================================================================================
subroutine flexInit(nxPad,nyPad)

! Arguments:
!! nx,ny = Dimensions of the padded grid used in the flexure FFT

! Global vars used:
!! planFwd, planInv = FFTW plan variables, pre-computed HERE for speed
!! sLoadPad, wLoadPad = FFTW padded working arrays

! Notes: 
!! FFTW creates an optimized plan for the specific FFT to be computed. The integer flag planType 
!! determines the throughness of the search and the speed of the resulting algorithm.

integer, intent(in) :: nxPad, nyPad

! Planner flag
!integer, parameter :: planType = FFTW_ESTIMATE 	! quick & dirty
integer, parameter :: planType = FFTW_MEASURE		! balanced	
!integer, parameter :: planType = FFTW_PATIENT		! slower, more optimized
!integer, parameter :: planType = FFTW_EXHAUSTIVE	! slowest, most optimized	

allocate( sLoadPad(nxPad,nyPad), wLoadPad(nxPad/2+1,nyPad) )
planFwd = fftw_plan_dft_r2c_2d( nyPad, nxPad, sLoadPad, wLoadPad, planType )
planInv = fftw_plan_dft_c2r_2d( nyPad, nxPad, wLoadPad, sLoadPad, planType )

return
end subroutine flexInit

! ==================================================================================================
! flex: compute flexural response to a combined ice & crustal rock load
! ==================================================================================================
subroutine flex( rx, ice, dx, dy, rho_crust, rho_ice, rho_mantle, ym, nu, te, defl )

! Arguments:
!! rx = crustal load, as 2D grid of heights, m
!! ice = ice load, as 2D grid of heights, m
!! dx, dy = grid spacing in the x- and y-directions, m
!! rho_crust, rho_ice, rho_mantle = densities in kg/m^3
!! ym = Young's modulus, Pa
!! nu = Poisson's ratio, nondim
!! te = elastic thickness, m
!! nxPad, nyPad = the size of the padded grid, must be at least as large as the rx and ice grids
!! defl = deflection, as a 2D grid of hieghts, such that (final topography) = (initial topo)+defl

! Global vars used:
!! planFwd, planInv = FFTW plan variables
!! sLoadPad, wLoadPad = FFTW padded working arrays

real(dp), intent(in) :: rx(:,:), ice(:,:), dx, dy, rho_crust, rho_ice, rho_mantle, ym, nu, te
real(dp), intent(out) :: defl(:,:)

integer :: nx, ny, nxPad, nyPad, nhPad, i, j
real(dp) :: D, twopi, ksq, wx, wy
real(dp), allocatable :: load(:,:), kx(:), ky(:)

! Get grid dimensions
nx = size(rx,1)
ny = size(rx,2)
nxPad = size(sLoadPad,1)
nyPad = size(sLoadPad,2)
nhPad = nxPad/2+1 ! the real forward FFT returns a half-grid for the first dimension
allocate( load(nx,ny), kx(nhPad), ky(nyPad) )

! Get the load as an equivalent height of crustal rock
load = rx+ice*(rho_ice/rho_crust)

! Add edge padding 
!! Note: Needed to satisfy assumed periodic boundaries for the fourier transform. I implement a 
!! version of the "expand" padding described by Blakley, Potential Theory in Gravity and Magnetic 
!! Applications, 1996, Appendix B
!!! Copy interior points
sLoadPad = 0._dp
sLoadPad(1:nx,1:ny) = load;
!!! Right pad
do i=nx+1,nxPad
    wx = real(i-nx-1)/real(nxPad-nx-1);
    sLoadPad(i,1:ny) = wx*sLoadPad(1,1:ny) + (1-wx)*sLoadPad(nx,1:ny);
end do
!!! Top pad
do j=ny+1,nyPad
    wy = real(j-ny-1)/real(nyPad-ny-1);
    sLoadPad(1:nx,j) = wy*sLoadPad(1:nx,1) + (1-wy)*sLoadPad(1:nx,ny);
end do
!!!Corner pad
do i=nx+1,nxPad
    do j=ny+1,nyPad
        wx = real(i-nx-1)/real(nxPad-nx-1);
        wy = real(j-ny-1)/real(nyPad-ny-1);
        sLoadPad(i,j) = ( wx*sLoadPad(1,j)+(1-wx)*sLoadPad(nx,j) + &
						wy*sLoadPad(i,1)+(1-wy)*sLoadPad(i,ny) )/2.;
    end do
end do

! Execute the forward transform
call fftw_execute_dft_r2c( planFwd, sLoadPad, wLoadPad )

! Apply the flexural isostasy filter
!! Compute wave numbers in x- and y-directions
!!! x-dir contains only positive k 
do i = 1,nhPad 
	kx(i) = real(i-1)/(dx*nxPad)
end do
!!! y contains both positive and negative k
do i = 0,nyPad/2
	ky(i+1) = real(i)/(dy*nyPad)
end do 
do i = nyPad/2+1,nyPad-1
	ky(i+1) = -real(nyPad-i)/(dy*nyPad)
end do
!! Filter
!!! Note: The equation comes from Watts (2001, pg 177, eq. 5.2), rho_infill and rho_fluid are set 
!!! to 0., which is correct for computing deflection due to a subariel load.
D = ym*te**3./(12.*(1-nu**2.))
twopi = atan(1._dp)*8._dp 
do i=1,nhPad
	do j=1,nyPad
		ksq = (twopi*kx(i))**2+(twopi*ky(j))**2 ! wave number (in rad/m), squared
		wLoadPad(i,j) = wLoadPad(i,j)*(rho_crust/rho_mantle)/( (D*ksq**2.)/(rho_mantle*9.81)+1. )
	end do
end do

! Execute the reverse transform
call fftw_execute_dft_c2r( planInv, wLoadPad, sLoadPad )

! Scale and remove padding
defl = sLoadPad(1:nx,1:ny)/(nxPad*nyPad) 

! Done
deallocate( load )

return
end subroutine flex

end module
