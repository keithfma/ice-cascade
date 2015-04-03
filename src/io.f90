! =============================================================================
! Input/Output routines for the ice-cascade landscape evolution model.
!
! Contains:
!   readParams (public): read from the input file specified on the command line
!   
! ============================================================================

module io_module

use types, only: sp, dp, sp_nc, dp_nc, int_nc
use grid_module, only: grid_type
use time_module, only: time_type
use hill_module, only: hill_type
use topo_module, only: topo_type
use netcdf

implicit none
private
public :: readParam, createOutfile, writeStep


  ! ---------------------------------------------------------------------------
  ! PARAMETERS
  ! ---------------------------------------------------------------------------
  integer, parameter :: p = dp       ! real kind for output
  integer, parameter :: p_nc = dp_nc ! netcdf real kind for output


contains

  ! ---------------------------------------------------------------------------
  ! SUB: read from the input file specified on the command line
  ! ---------------------------------------------------------------------------
  subroutine readParam (runname, fgrid, time, ftopo, fhill)
    
    character(len=*), intent(out) :: runname ! name for run
    type(grid_type), intent(out) :: fgrid    ! high-res grid
    type(time_type), intent(out) :: time     ! model time vars
    type(topo_type), intent(out) :: ftopo    ! high-res topography
    type(hill_type), intent(out) :: fhill    ! hilllslope model

    character(len=100) :: infile, line
    integer :: tmp, msg
  	
    ! Get input file name
    select case (command_argument_count())
      case (1)
      	call get_command_argument(1,infile) 
      case default
      	print*,'ICE-CASCADE expects exactly 1 input argument'
      	stop
    end select
    
    ! Copy input to scratch file, dropping comments and empty lines
    open(54, file = trim(infile), status = 'old', iostat = msg)
    if (msg .ne. 0) then
    	print*, 'The input file ', trim(infile), ' does not exist, exiting.'
    	stop
    end if	
    open (55, status = 'scratch')
    do while (msg .ge. 0)
    	read (54, *, iostat = msg) line
    	if ((line(1:1) .ne. '$') .and. (line(1:1) .ne. ' ')) write (55, '(a)') line		
    enddo 
    close (54)
    rewind (55)
    
    ! Read in input parameters
    read (55,*) runname	
    read (55,*) time%start
    read (55,*) time%finish
    read (55,*) time%step
    read (55,*) time%write_period
    read (55,*) fgrid%nx	
    ftopo%nx = fgrid%nx
    fhill%nx = fgrid%nx
    read (55,*) fgrid%ny
    ftopo%ny = fgrid%ny
    fhill%ny = fgrid%ny
    read (55,*) fgrid%dx
    fhill%dx = fgrid%dx
    read (55,*) fgrid%dy	
    fhill%dy = fgrid%dy
    read (55,*) ftopo%filename
    read (55,*) ftopo%write_z
    read (55,*) tmp; fhill%on = merge(.true., .false., tmp==1)
    read (55,*) fhill%D	
    read (55,*) fhill%nbcName
    read (55,*) fhill%sbcName 
    read (55,*) fhill%ebcName
    read (55,*) fhill%wbcName 
    read (55,*) fhill%solnName 	
    read (55,*) tmp; fhill%write_dzdt = merge(.true., .false., tmp==1)
    close(55)
    
  end subroutine readParam


  ! ---------------------------------------------------------------------------
  ! SUB: create netcdf file and vars for output
  ! ---------------------------------------------------------------------------
  subroutine createOutfile(runname, fgrid, time, ftopo, fhill)

    character(len=*), intent(in) :: runname
    type(grid_type), intent(in)  :: fgrid
    type(time_type), intent(in)  :: time
    type(topo_type), intent(in)  :: ftopo
    type(hill_type), intent(in)  :: fhill

    logical :: shuf
    integer, dimension(3) :: fchunk
    integer :: i, j, defLvl, msg, id_file, id_dim_fx, id_dim_fy, &
      id_dim_time, id_var
 
    ! define compression and chunking parameters 
    defLvl = 1 ! compression, 0 = none, 9 = max, best value is 1
    fchunk = [fgrid%nx, fgrid%ny, 1]
    shuf = .true.

    ! create new file
    msg = nf90_create(trim(runname)//'.out', nf90_netcdf4, id_file)

    ! write parameters as global attributes
    msg = nf90_put_att(id_file, nf90_global, 'model_start_time__a', time%start)
    msg = nf90_put_att(id_file, nf90_global, 'model_end_time__a', time%finish)
    msg = nf90_put_att(id_file, nf90_global, 'model_time_step__a', time%step)
    msg = nf90_put_att(id_file, nf90_global, 'output_interval__steps', time%write_period)
    msg = nf90_put_att(id_file, nf90_global, 'grid_high_res_nx__1', fgrid%nx)
    msg = nf90_put_att(id_file, nf90_global, 'grid_high_res_ny__1', fgrid%ny)
    msg = nf90_put_att(id_file, nf90_global, 'grid_high_res_dx__m', fgrid%dx)
    msg = nf90_put_att(id_file, nf90_global, 'grid_high_res_dy__m', fgrid%dy)
    if (fhill%on) then
      msg = nf90_put_att(id_file, nf90_global, 'hill_diffusivity__m2a-1', fhill%D)
      msg = nf90_put_att(id_file, nf90_global, 'hill_north_bc', trim(fhill%nbcName))
      msg = nf90_put_att(id_file, nf90_global, 'hill_south_bc', trim(fhill%sbcName))
      msg = nf90_put_att(id_file, nf90_global, 'hill_west_bc', trim(fhill%wbcName))
      msg = nf90_put_att(id_file, nf90_global, 'hill_east_bc', trim(fhill%ebcName))
      msg = nf90_put_att(id_file, nf90_global, 'hill_topo_soln', trim(fhill%solnName))
    end if

    ! define dimensions
    msg = nf90_def_dim(id_file, 'fx', fgrid%nx, id_dim_fx) 
    msg = nf90_def_dim(id_file, 'fy', fgrid%ny, id_dim_fy) 
    msg = nf90_def_dim(id_file, 'time', nf90_unlimited, id_dim_time)

    ! create variables
    msg = nf90_def_var(id_file, 'time', p_nc, id_dim_time, id_var)
    msg = nf90_put_att(id_file, id_var, 'long_name', 'model_time')
    msg = nf90_put_att(id_file, id_var, 'units', 'a')

    msg = nf90_def_var(id_file, 'fx', p_nc, id_dim_fx, id_var)
    msg = nf90_put_att(id_file, id_var, 'long_name', 'x_coord_high_res')
    msg = nf90_put_att(id_file, id_var, 'units', 'm')
    
    msg = nf90_def_var(id_file, 'fy', p_nc, id_dim_fy, id_var)
    msg = nf90_put_att(id_file, id_var, 'long_name', 'y_coord_high_res')
    msg = nf90_put_att(id_file, id_var, 'units', 'm')

    if (ftopo%write_z) then
     	msg = nf90_def_var(id_file, 'ftopo_z', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'topography_high_res')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm')
    end if

    if (fhill%write_dzdt) then
     	msg = nf90_def_var(id_file, 'fhill_dzdt', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'hillslope_dzdt_high_res')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm')
    end if

    if (fhill%write_soln) then
     	msg = nf90_def_var(id_file, 'fhill_soln', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'hillslope_topo_solution_high_res')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm')
    end if

    ! exit definition mode
    msg = nf90_enddef(id_file) 

    ! populate dimension variables
    msg = nf90_inq_varid(id_file, 'fx', id_var)
    msg = nf90_put_var(id_file, id_var, fgrid%x(2:fgrid%nx+1))

    msg = nf90_inq_varid(id_file, 'fy', id_var)
    msg = nf90_put_var(id_file, id_var, fgrid%y(2:fgrid%ny+1))

    ! close file
    msg = nf90_close(id_file)

  end subroutine createOutfile

  ! ---------------------------------------------------------------------------
  ! SUB: write data to output file for this step
  ! ---------------------------------------------------------------------------
  subroutine writeStep(runname, time, ftopo, fhill)
  
    character(len=*), intent(in)    :: runname
    type(time_type), intent(inout)  :: time
    type(topo_type), intent(in)     :: ftopo
    type(hill_type), intent(in)     :: fhill

    integer :: fx0, fx1, fy0, fy1, msg, id_file, id_var

    ! increment step counter
    time%out_step = time%out_step+1

    ! prompt user
    print *, 'TIME = ', time%now

    ! define limits of interior points, for convenience
    fx0 = 2
    fx1 = ftopo%nx+1
    fy0 = 2
    fy1 = ftopo%ny+1

    ! open file
    msg = nf90_open(trim(runname)//'.out', nf90_write, id_file)

    ! write data
    msg = nf90_inq_varid(id_file, 'time', id_var)
    msg = nf90_put_var(id_file, id_var, real(time%now, p), [time%out_step] )

    if (ftopo%write_z) then
      msg = nf90_inq_varid(id_file, 'ftopo_z', id_var)
      msg = nf90_put_var(id_file, id_var, real(ftopo%z(fx0:fx1, fy0:fy1), p), [1, 1, time%out_step] )
    end if

    ! close file
    msg = nf90_close(id_file)

  end subroutine writeStep

!! Write output to netcdf 
!msg = nf90_put_var(oFile, oTime, time, (/ step /) )
!
!if (pWriteFlag(1)==1) then
!	msg = nf90_put_var(oFile, olH, real(lH(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(2)==1) then
!	msg = nf90_put_var(oFile, ofH, real(fH,sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(3)==1) then
!	msg = nf90_put_var(oFile, olT, real(lT(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(4)==1) then
!	msg = nf90_put_var(oFile, ofT, real(fT,sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(5)==1) then
!	msg = nf90_put_var(oFile, olHT, real(lHT(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(6)==1) then
!	msg = nf90_put_var(oFile, ofHT, real(fHT,sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(7)==1) then	
!	msg = nf90_put_var(oFile, olTempS, real(lTempS(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )	
!end if
!
!if (pWriteFlag(8)==1) then
!	msg = nf90_put_var(oFile, olTempB, real(lTempB(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(9)==1) then
!	msg = nf90_put_var(oFile, olTempM, real(lTempM(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(10)==1) then
!	msg = nf90_put_var(oFile, olUDefm, real(lUDefm(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(11)==1) then
!	msg = nf90_put_var(oFile, olVDefm, real(lVDefm(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(12)==1) then
!	msg = nf90_put_var(oFile, olUSlid, real(lUSlid(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(13)==1) then
!	msg = nf90_put_var(oFile, olVSlid, real(lVSlid(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(14)==1) then
!	msg = nf90_put_var(oFile, olSliding, real(lSliding(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(15)==1) then
!	msg = nf90_put_var(oFile, ofSliding, real(fSliding,sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(16)==1) then
!	msg = nf90_put_var(oFile, olConstrict, real(lConstrict(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(18)==1) then
!	msg = nf90_put_var(oFile, ofGlacErosRate, real(fGlacErosRate,sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(19)==1) then
!	msg = nf90_put_var(oFile, ofhillErosRate, real(fhillErosRate,sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(20)==1) then
!	msg = nf90_put_var(oFile, ofSlope, real(fSlope,sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(21)==1) then
!	msg = nf90_put_var(oFile, ofQWater, real(fQWater,sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(22)==1) then
!	msg = nf90_put_var(oFile, ofFluvErosRate, real(fFluvErosRate,sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(23)==1) then
!	msg = nf90_put_var(oFile, ofWater, real(fWater,sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(24)==1) then
!	msg = nf90_put_var(oFile, oDvolIceSrc, real(dvolIceSrc,sp), (/ step /) )
!	msg = nf90_put_var(oFile, oDvolIceSnk, real(dvolIceSnk,sp), (/ step /) )
!	msg = nf90_put_var(oFile, oDvolIce, real(dvolIce,sp), (/ step /) )	
!end if
!
!if (pWriteFlag(27)==1) then
!	msg = nf90_put_var(oFile, olBalRate, real(lBalRate(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(28)==1) then
!	msg = nf90_put_var(oFile, ofLake, merge(1, 0, fLake), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(29)==1) then
!	msg = nf90_put_var(oFile, ofCatchment, fCatchment, (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(30)==1) then
!	msg = nf90_put_var(oFile, oTimeStepIceMean, timeStepIceMean, (/ step /) )
!end if
!
!if (pWriteFlag(31)==1) then
!	msg = nf90_put_var(oFile, oTempSl, tempSl, (/ step /) )
!end if
!
!if (pWriteFlag(32)==1) then
!	msg = nf90_put_var(oFile, ofFlex, real(fFlex,sp), (/ 1, 1, step /) )
!end if
!
!if (pWriteFlag(33)==1) then
!	msg = nf90_put_var(oFile, ofSolnH, real(fSolnH,sp), (/ 1, 1, step /) )
!end if
!
!return
!end subroutine writeStep
!
!! ==================================================================================================
!! closeOutput: close the output file.
!! ==================================================================================================
!subroutine closeOutput ()
!
!integer msg
!
!msg = nf90_close(oFile)
!return
!
!end subroutine closeOutput
!
!! ==================================================================================================
!! debugOut2d: Writes a single 2D array to file, used for debugging purposes
!! ==================================================================================================
!subroutine debugOut2d(arr,filename)
!
!character(len=*), intent(in) :: filename
!real(dp), intent(in) :: arr(:,:)
!
!! Arguments:
!!! arr (in): array to be written, must be 2D double precision
!!! filename (in): string containing the output filename
!
!integer :: nx, ny, msg, outId, xDimId, yDimId, arrVarId
!
!nx = size(arr,1)
!ny = size(arr,2)
!
!msg = nf90_create( trim(filename), nf90_netcdf4, outId )
!msg =  nf90_def_dim( outId, 'x', nx, xDimId ) 
!msg =  nf90_def_dim( outId, 'y', ny, yDimId ) 
!msg = nf90_def_var( outId, 'arr', nf90_double, (/ xDimId, yDimId /), arrVarId )
!msg = nf90_put_var( outId, arrVarId, arr, (/ 1, 1 /) )
!msg = nf90_close(outId)
!
!return
!end subroutine debugOut2d

end module io_module
