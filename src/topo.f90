! =============================================================================
! Topography for ICE-CASCADE model, including variables and procedures
!
! Contains:
!   type topo_type (public)
!   subroutine init (private, type-bound) 
!   subroutine readNetcdf (private)
!   sub smooth (NOT IMPLEMENTED)
!   sub noise (NOT IMPLEMENTED)
! ============================================================================

module topo_module

use types, only: dp
use netcdf

implicit none
private
public topo_type

  ! --------------------------------------------------------------------------- 
  ! TYPE: all variables and procedures for the hillslope model component
  ! ---------------------------------------------------------------------------
  type topo_type
    character(len=100)    :: filename ! input file name
    integer               :: nx       ! num grid points in x-dir, [1]
    integer               :: ny       ! num grid points in y-dir, [1]
    real(dp), allocatable :: z(:,:)   ! topography, [masl]
    logical               :: write_z  ! output flag
  contains
    procedure, pass       :: init     ! initialize all components
  end type topo_type

contains


  ! --------------------------------------------------------------------------- 
  ! SUB: initialize topo object
  !   Note: components nx, ny, filename, and write_z must be set before init
  ! --------------------------------------------------------------------------- 
  subroutine init(t)

    class(topo_type), intent(inout) :: t ! object to initialize

    if (allocated(t%z) .eqv. .true.) deallocate(t%z)
    allocate(t%z(t%nx+2, t%ny+2))
    t%z = 0.0_dp
    if (t%filename .ne. "zero") call readNetcdf(t)

  end subroutine init


  ! --------------------------------------------------------------------------- 
  ! SUB: read topo data from netcdf (expects GMT grid format)
  ! --------------------------------------------------------------------------- 
  subroutine readNetcdf(t)
    
    type(topo_type), intent(inout) :: t ! top object to modify

    integer :: ndim, nvar, n1, n2, zind, i, var_ndim
    integer :: msg, id_file

    ! open file
	  msg = nf90_open(trim(t%filename), nf90_nowrite, id_file)

    ! check dimensions
	  msg = nf90_inquire(id_file, ndim, nvar)	
	  if (ndim .ne. 2) then
		  print*,'Topo input file should have only two dimensions'
      stop
    end if	
		msg = nf90_inquire_dimension(id_file, 1, len = n1)	
		if (n1 .ne. t%nx) then
      print*,'Topo input data does not match the given dimensions (nx)'
      stop
    end if
		msg = nf90_inquire_dimension(id_file, 2, len = n2)	
		if (n2 .ne. t%ny) then
      print*,'Topo input data does not match the given dimensions (ny)'
      stop
    end if
    
    ! identify and read topography variable
    zind = -1
    do i = 1, nvar
			msg = nf90_inquire_variable(id_file, i, ndims = var_ndim)
		  if ((var_ndim .eq. 2) .and. (zind .ne. -1)) then
				print*,'Topo input file should only have one 2D variable'
        stop
      end if
      if (var_ndim .eq. 2) zind = i
  	end do	
	  msg = nf90_get_var(id_file, zind, t%z(2:t%nx+1, 2:t%ny+1))
    
    ! close file
    msg = nf90_close(id_file)

  end subroutine readNetcdf

end module topo_module
