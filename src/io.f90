! =============================================================================
! Input/output variables and procedures routines for ice-cascade.
!
! Contains:
!   io_type: Public, object for io vars and methods
!   
! ============================================================================

module io_mod

use kinds_mod, only: rp
use common_mod, only: common_type
use time_mod, only: time_type
use netcdf

implicit none
private
public :: io_type


  ! ---------------------------------------------------------------------------
  ! TYPE: input/output vars and procedures
  ! ---------------------------------------------------------------------------
  type io_type
    character(len=100) :: name_run ! user defined run name
    character(len=100) :: name_out ! output file name
    character(len=100) :: name_T ! initial topography name
    logical :: write_T 
  contains
    procedure, pass :: read_param ! read parameters from the input file
    procedure, pass :: read_initial_vals ! read initial values 
  end type io_type


contains


  ! ---------------------------------------------------------------------------
  ! SUB: Read input parameters from file
  ! ---------------------------------------------------------------------------
  subroutine read_param(w, t, c)

    class(io_type), intent(out) :: w
    type(time_type), intent(out) :: t
    type(common_type), intent(out) :: c

    character(len=100) :: infile, line
    integer :: msg

    ! Get input file name from command line
    select case (command_argument_count())
      case (1)
      	call get_command_argument(1,infile) 
      case default
      	print*,'ICE-CASCADE expects exactly 1 input argument'
      	stop -1
    end select
    
    ! Sanitize input file (drop comments and empty lines)
    open(54, file = trim(infile), status = 'old', iostat = msg)
    if (msg .ne. 0) then
    	print*, 'The input file ', trim(infile), ' does not exist, exiting.'
    	stop
    end if	
    open (55, status = 'scratch')
    do while (msg .ge. 0)
    	read (54, '(a)', iostat = msg) line
    	if ((line(1:1) .ne. '$') .and. (line(1:1) .ne. ' ')) write (55, '(a)') line		
    enddo 
    close (54)
    rewind (55)
    
    ! Read input parameters
    read(55, *) w%name_run	
    read(55, *) t%start
    read(55, *) t%finish
    read(55, *) t%step
    read(55, *) t%step_out
    read(55, *) c%rhoi
    read(55, *) c%nx	
    read(55, *) c%ny
    read(55, *) c%dx
    read(55, *) c%dy	
    read(55, *) w%name_T 
    read(55, *) w%write_T

  end subroutine read_param
  

  ! ---------------------------------------------------------------------------
  ! SUB: read the initial values for state variables
  ! ---------------------------------------------------------------------------
  subroutine read_initial_vals(w, c)

    class(io_type), intent(in) :: w
    type(common_type), intent(inout) :: c

    ! initial topography
    select case(w%name_T)
    case('zero')
      c%T = 0.0_rp
    case default
      call read_array_nc(w%name_T, c%nx, c%ny, c%T)
    end select

  end subroutine read_initial_vals


  ! --------------------------------------------------------------------------- 
  ! SUB: read 2D array from GMT grd format netcdf
  ! --------------------------------------------------------------------------- 
  subroutine read_array_nc(filename, nx, ny, array)
    
    character(len=*), intent(in) :: filename
    integer, intent(in) :: nx, ny
    real(rp), intent(out) :: array(:,:)

    integer :: ndim, nvar, n1, n2, ind, i, var_ndim
    integer :: msg, id_file

    ! open file
	  msg = nf90_open(trim(filename), nf90_nowrite, id_file)

    ! check dimensions
	  msg = nf90_inquire(id_file, ndim, nvar)	
	  if (ndim .ne. 2) then
		  print *,'Invalid input data: netcdf file should have only two dimensions'
      stop -1
    end if	
		msg = nf90_inquire_dimension(id_file, 1, len = n1)	
		if (n1 .ne. nx) then
      print *,'Invalid input data: data does not match the given dimensions (nx)'
      stop -1
    end if
		msg = nf90_inquire_dimension(id_file, 2, len = n2)	
		if (n2 .ne. ny) then
      print *,'Invalid input data: data does not match the given dimensions (ny)'
      stop -1
    end if
    
    ! identify array variable 
    ind = -1
    do i = 1, nvar
			msg = nf90_inquire_variable(id_file, i, ndims = var_ndim)
		  if ((var_ndim .eq. 2) .and. (ind .ne. -1)) then
				print*,'Invalid input data: netcdf file should only have one 2D variable'
        stop -1
      end if
      if (var_ndim .eq. 2) ind = i
  	end do	

    ! read array to interior points
	  msg = nf90_get_var(id_file, ind, array(2:nx+1, 2:ny+1))
    
    ! close file
    msg = nf90_close(id_file)

  end subroutine read_array_nc


end module io_mod
