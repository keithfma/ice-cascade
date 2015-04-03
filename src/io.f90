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
public :: readParam, createOutfile


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
      id_dim_time, id_var_time, id_var_fx, id_var_fy, id_var_ftopo_z, &
      id_var_fhill_dzdt, id_var_fhill_soln 
 
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
    msg = nf90_def_var( id_file, 'time', p_nc, id_dim_time, id_var_time)
    msg = nf90_put_att( id_file, id_var_time, 'long_name', 'model_time')
    msg = nf90_put_att( id_file, id_var_time, 'units', 'a')

    msg = nf90_def_var( id_file, 'fx', p_nc, id_dim_fx, id_var_fx )
    msg = nf90_put_att( id_file, id_var_fx, 'long_name', 'x_coord_high_res')
    msg = nf90_put_att( id_file, id_var_fx, 'units', 'm')
    
    msg = nf90_def_var( id_file, 'fy', p_nc, id_dim_fy, id_var_fy )
    msg = nf90_put_att( id_file, id_var_fy, 'long_name', 'y_coord_high_res')
    msg = nf90_put_att( id_file, id_var_fy, 'units', 'm')

    if (ftopo%write_z) then
     	msg = nf90_def_var(id_file, 'ftopo_z', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var_ftopo_z, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var_ftopo_z, 'long_name', 'topography_high_res')
     	msg = nf90_put_att(id_file, id_var_ftopo_z, 'units', 'm')
    end if

    if (fhill%write_dzdt) then
     	msg = nf90_def_var(id_file, 'fhill_dzdt', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var_fhill_dzdt, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var_fhill_dzdt, 'long_name', 'hillslope_dzdt_high_res')
     	msg = nf90_put_att(id_file, id_var_fhill_dzdt, 'units', 'm')
    end if

    if (fhill%write_soln) then
     	msg = nf90_def_var(id_file, 'fhill_soln', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var_fhill_soln, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var_fhill_soln, 'long_name', 'hillslope_topo_solution_high_res')
     	msg = nf90_put_att(id_file, id_var_fhill_soln, 'units', 'm')
    end if

    ! exit definition mode
    msg = nf90_enddef(id_file) 

    ! populate dimension variables
    msg = nf90_put_var(id_file, id_var_fx, fgrid%x(2:fgrid%nx+1))
    msg = nf90_put_var(id_file, id_var_fy, fgrid%y(2:fgrid%ny+1))

    
    !do i = 1,lNx
!	msg = nf90_put_var( oFile, olX, (i-1)*lDx, (/ i /) )
!end do

  end subroutine createOutfile

!
!!! Create variables
!
!
!
!if (pWriteFlag(1)==1) then
!	msg = nf90_def_var( oFile, 'lH', sp_nc, (/ olXDim, olYDim, oTimeDim /), olH, &
!		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, olH, 'long_name', 'ice thickness, low resolution grid')
!	msg = nf90_put_att( oFile, olH, 'units', 'm')
!	msg = nf90_put_att( oFile, olH, 'valid_min', 0._dp)
!end if	
!
!if (pWriteFlag(2)==1) then
!	msg = nf90_def_var( oFile, 'fH', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofH, &
!		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofH, 'long_name', 'ice thickness, high resolution grid')
!	msg = nf90_put_att( oFile, ofH, 'units', 'm')
!	msg = nf90_put_att( oFile, ofH, 'valid_min', 0._dp)
!end if	
!	
!if (pWriteFlag(3)==1) then	
!	msg = nf90_def_var( oFile, 'lT', sp_nc, (/ olXDim, olYDim, oTimeDim /), olT, &
!		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, olT, 'long_name', 'bedrock elevation, low resolution grid')
!	msg = nf90_put_att( oFile, olT, 'units', 'm')
!end if
!
!if (pWriteFlag(4)==1) then	
!	msg = nf90_def_var( oFile, 'fT', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofT, &
!		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofT, 'long_name', 'bedrock elevation, high resolution grid')
!	msg = nf90_put_att( oFile, ofT, 'units', 'm')
!end if
!
!if (pWriteFlag(5)==1) then
!	msg = nf90_def_var( oFile, 'lHT', sp_nc, (/ olXDim, olYDim, oTimeDim /), olHT, &
!		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, olHT, 'long_name', 'ice surface topography, low resolution grid')
!	msg = nf90_put_att( oFile, olHT, 'units', 'm')
!end if	
!
!if (pWriteFlag(6)==1) then
!	msg = nf90_def_var( oFile, 'fHT', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofHT, &
!		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofHT, 'long_name', 'ice surface topography, high resolution grid')
!	msg = nf90_put_att( oFile, ofHT, 'units', 'm')
!end if	
!
!if (pWriteFlag(7)==1) then
!	msg = nf90_def_var( oFile, 'lTempS', sp_nc, (/ olXDim, olYDim, oTimeDim /), olTempS, &
!		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, olTempS, 'long_name', 'surface temperature, low resolution grid')
!	msg = nf90_put_att( oFile, olTempS, 'units', 'degrees C')
!end if
!
!if (pWriteFlag(8)==1) then
!	msg = nf90_def_var( oFile, 'lTempB', sp_nc, (/ olXDim, olYDim, oTimeDim /), olTempB, &
!		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, olTempB, 'long_name', 'basal temperature, low resolution grid')
!	msg = nf90_put_att( oFile, olTempB, 'units', 'degrees C')
!end if
!
!if (pWriteFlag(9)==1) then
!	msg = nf90_def_var( oFile, 'lTempM', sp_nc, (/ olXDim, olYDim, oTimeDim /), olTempM, &
!		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, olTempM, 'long_name', &
!		'ice melting temperature, low resolution grid')
!	msg = nf90_put_att( oFile, olTempM, 'units', 'degrees C')	
!end if
!
!if (pWriteFlag(10)==1) then
!	msg = nf90_def_var( oFile, 'lUDefm', sp_nc, (/ olXDim, olYDim, oTimeDim /), olUDefm, &
!		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, olUDefm, 'long_name', 'ice deformation velocity, x-component, low resolution grid')
!	msg = nf90_put_att( oFile, olUDefm, 'units', 'm/yr')	
!end if
!
!if (pWriteFlag(11)==1) then
!	msg = nf90_def_var( oFile, 'lVDefm', sp_nc, (/ olXDim, olYDim, oTimeDim /), olVDefm, &
!		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, olUDefm, 'long_name', 'ice deformation velocity, y-component, low resolution grid')
!	msg = nf90_put_att( oFile, olVDefm, 'units', 'm/yr')	
!end if
!
!if (pWriteFlag(12)==1) then
!	msg = nf90_def_var( oFile, 'lUSlid', sp_nc, (/ olXDim, olYDim, oTimeDim /), olUSlid, &
!		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, olUSlid, 'long_name', 'ice sliding velocity, x-component, low resolution grid')
!	msg = nf90_put_att( oFile, olUSlid, 'units', 'm/yr')	
!end if
!
!if (pWriteFlag(13)==1) then
!	msg = nf90_def_var( oFile, 'lVSlid', sp_nc, (/ olXDim, olYDim, oTimeDim /), olVSlid, &
!		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, olVSlid, 'long_name', 'ice sliding velocity, y-component, low resolution grid')
!	msg = nf90_put_att( oFile, olVSlid, 'units', 'm/yr')	
!end if
!
!if (pWriteFlag(14)==1) then
!	msg = nf90_def_var( oFile, 'lSliding', sp_nc, (/ olXDim, olYDim, oTimeDim /), olSliding, &
!		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, olSliding, 'long_name', 'ice sliding velocity magnitude, low resolution grid')
!	msg = nf90_put_att( oFile, olSliding, 'units', 'm/yr')	
!end if
!
!if (pWriteFlag(15)==1) then	
!	msg = nf90_def_var( oFile, 'fSliding', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofSliding, &
!		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofSliding, 'long_name', 'ice sliding velocity magnitude, high resolution grid')
!	msg = nf90_put_att( oFile, ofSliding, 'units', 'm/yr')
!end if
!
!if (pWriteFlag(16)==1) then
!	msg = nf90_def_var( oFile, 'lConstrict', sp_nc, (/ olXDim, olYDim, oTimeDim /), olConstrict, &
!		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, olConstrict, 'long_name', 'ice constriction factor, low resolution grid')
!	msg = nf90_put_att( oFile, olConstrict, 'units', 'non-dimensional')	
!end if
!
!if (pWriteFlag(17)==1) then	
!	msg = nf90_def_var( oFile, 'fUpliftRate', sp_nc, (/ ofXDim, ofYDim /), ofUpliftRate, &
!		chunksizes = (/ fNx, fNy /), shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofUpliftRate, 'long_name', 'tectonic uplift rate,  high resolution grid')
!	msg = nf90_put_att( oFile, ofUpliftRate, 'units', 'm/yr')
!end if
!
!if (pWriteFlag(18)==1) then	
!	msg = nf90_def_var( oFile, 'fGlacErosRate', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofGlacErosRate, &
!		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofGlacErosRate, 'long_name', 'glacial erosion rate,  high resolution grid')
!	msg = nf90_put_att( oFile, ofGlacErosRate, 'units', 'm/yr')
!end if
!
!if (pWriteFlag(19)==1) then	
!	msg = nf90_def_var( oFile, 'fhillErosRate', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofhillErosRate, &
!		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofhillErosRate, 'long_name', 'hillslope erosion rate,  high resolution grid')
!	msg = nf90_put_att( oFile, ofhillErosRate, 'units', 'm/yr')
!end if
!
!if (pWriteFlag(20)==1) then	
!	msg = nf90_def_var( oFile, 'fSlope', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofSlope, &
!		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofSlope, 'long_name', 'bedrock slope,  high resolution grid')
!	msg = nf90_put_att( oFile, ofSlope, 'units', 'nondim')
!end if
!
!if (pWriteFlag(21)==1) then	
!	msg = nf90_def_var( oFile, 'fQWater', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofQWater, &
!		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofQWater, 'long_name', 'water discharge, high resolution grid')
!	msg = nf90_put_att( oFile, ofQWater, 'units', 'm^3/yr')
!end if
!
!if (pWriteFlag(22)==1) then	
!	msg = nf90_def_var( oFile, 'fFluvErosRate', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofFluvErosRate, &
!		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofFluvErosRate, 'long_name', 'fluvial erosion rate,  high resolution grid')
!	msg = nf90_put_att( oFile, ofFluvErosRate, 'units', 'm/yr')
!end if
!
!if (pWriteFlag(23)==1) then	
!	msg = nf90_def_var( oFile, 'fWater', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofWater, &
!		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofWater, 'long_name', 'water input rate (rainfall + ice melt),  high resolution grid')
!	msg = nf90_put_att( oFile, ofWater, 'units', 'm/yr')
!end if
!
!if (pWriteFlag(24)==1) then
!	
!	msg = nf90_def_var( oFile, 'dvolIceSrc', sp_nc, (/ oTimeDim /), oDvolIceSrc )
!	msg = nf90_put_att( oFile, oDvolIceSrc, 'long_name', &
!		'ice volume added by mass balance and flux at domain boudaries for each step,  scalar')
!	msg = nf90_put_att( oFile, oDvolIceSrc, 'units', 'm^3')
!	
!	msg = nf90_def_var( oFile, 'dvolIceSnk', sp_nc, (/ oTimeDim /), oDvolIceSnk )
!	msg = nf90_put_att( oFile, oDvolIceSnk, 'long_name', &
!		'ice volume lost by mass balance and flux at domain boudaries for each step, scalar')
!	msg = nf90_put_att( oFile, oDvolIceSnk, 'units', 'm^3')
!
!	msg = nf90_def_var( oFile, 'dvolIce', sp_nc, (/ oTimeDim /), oDvolIce )
!	msg = nf90_put_att( oFile, oDvolIce, 'long_name', 'change in ice volume for each step, scalar')
!	msg = nf90_put_att( oFile, oDvolIce, 'units', 'm^3')
!
!end if
!
!!! Note: dropped pWriteFlag(25), this variable was used for lHSoln
!
!if (pWriteFlag(26)==1) then
!	msg = nf90_def_var( oFile, 'fSolnHss', sp_nc, (/ ofXDim, ofYDim /), ofSolnHss, &
!		chunksizes = (/ fNx, fNy /), shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofSolnHss, 'long_name', &
!		'Exact solution for ice thickness at steady state,  high resolution grid')
!	msg = nf90_put_att( oFile, ofSolnHss, 'units', 'm')
!end if
!
!if (pWriteFlag(27)==1) then
!	msg = nf90_def_var( oFile, 'lBalRate', sp_nc, (/ olXDim, olYDim, oTimeDim /), olBalRate, &
!		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, olBalRate, 'long_name', 'balance rate, low resolution grid')
!	msg = nf90_put_att( oFile, olBalRate, 'units', 'm ice / yr')
!end if
!
!if (pWriteFlag(28)==1) then
!	msg = nf90_def_var( oFile, 'fLake', int_nc, (/ ofXDim, ofYDim, oTimeDim /), ofLake, &
!		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofLake, 'long_name', 'lakes, high resolution grid')
!	msg = nf90_put_att( oFile, ofLake, 'units', 'true/false')
!end if
!
!if (pWriteFlag(29)==1) then
!	msg = nf90_def_var( oFile, 'fCatchment', int_nc, (/ ofXDim, ofYDim, oTimeDim /), ofCatchment, &
!		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofCatchment, 'long_name', 'fluvial catchments, high resolution grid')
!	msg = nf90_put_att( oFile, ofCatchment, 'units', 'unique integer flag')
!end if
!
!if (pWriteFlag(30)==1) then
!	msg = nf90_def_var( oFile, 'timeStepIceMean', sp_nc, (/ oTimeDim /), oTimeStepIceMean )
!	msg = nf90_put_att( oFile, oTimeStepIceMean, 'long_name','mean time step for the ice model')
!	msg = nf90_put_att( oFile, oTimeStepIceMean, 'units', 'yr')
!end if
!
!if (pWriteFlag(31)==1) then
!	msg = nf90_def_var( oFile, 'tempSl', sp_nc, (/ oTimeDim /), oTempSl )
!	msg = nf90_put_att( oFile, oTempSl, 'long_name','sea level temperature, mean annual')
!	msg = nf90_put_att( oFile, oTempSl, 'units', 'deg C')
!end if
!
!if (pWriteFlag(32)==1) then
!	msg = nf90_def_var( oFile, 'fFlex', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofFlex, &
!		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofFlex, 'long_name', 'flexural isostatic deflection, high resolution grid')
!	msg = nf90_put_att( oFile, ofFlex, 'units', 'm')
!end if
!
!if (pWriteFlag(33)==1) then
!	msg = nf90_def_var( oFile, 'fSolnH', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofSolnH, &
!		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
!	msg = nf90_put_att( oFile, ofSolnH, 'long_name', &
!		'Exact solution for ice thickness, time dependant, high resolution grid')
!	msg = nf90_put_att( oFile, ofSolnH, 'units', 'm')
!end if
!
!
!!! Exit definition mode
!msg = nf90_enddef( oFile ) 
!
!!! Populate coordinates variables
!do i = 1,lNx
!	msg = nf90_put_var( oFile, olX, (i-1)*lDx, (/ i /) )
!end do
!
!do j = 1,lNy
!	msg = nf90_put_var( oFile, olY, (j-1)*lDy, (/ j /) )
!end do
!
!do i = 1,fNx
!	msg = nf90_put_var( oFile, ofX, (i-1)*fDx, (/ i /) )
!end do
!
!do j = 1,fNy
!	msg = nf90_put_var( oFile, ofY, (j-1)*fDy, (/ j /) )
!end do
!
!
!return
!end subroutine createOutput
!
!! ==================================================================================================
!! writeConst: writes constant variables to the output file, once
!! ==================================================================================================
!subroutine writeConst (fUpliftRate, fSolnHss, pWriteFlag)
!
!! Arguments:
!!! fUpliftRate (in) = tectonic uplift rate, m/yr
!!! fSolnHss (in) = high-res exact solution for steady-state ice thickness in benchmark cases, m
!!! pWriteFlag (in) = integer flags for which variables to write to output
!
!integer, intent(in) :: pWriteFlag(:)
!real(dp), intent(in) :: fUpliftRate(:,:), fSolnHss(:,:)
!integer :: msg
!
!if (pWriteFlag(17)==1) then
!	msg = nf90_put_var(oFile, ofUpliftRate, real(fUpliftRate,sp), (/ 1, 1 /) )
!end if
!
!if (pWriteFlag(26)==1) then
!	msg = nf90_put_var(oFile, ofSolnHss, real(fSolnHss,sp), (/ 1, 1 /) )
!end if
!
!return
!end subroutine writeConst
!
!! ==================================================================================================
!! writeStep: writes the values of time-dependent variables at this step to the output netcdf file
!! ==================================================================================================
!subroutine writeStep ( step, time, pWriteFlag, lH, fH, lT, fT, lHT, fHT, lTempS, lTempB, lTempM, &
!	lUDefm, lVDefm, lUSlid, lVSlid, lSliding, fSliding, lConstrict, fGlacErosRate, fhillErosRate, &
!	fSlope, fQWater, fFluvErosRate, fWater, dvolIceSrc, dvolIceSnk, dvolIce, lBalRate, fLake, &
!	fCatchment, timeStepIceMean, tempSl, fFlex, fSolnH )
!	
!! Arguments:
!!! step (in) = number of the current output step
!!! time (in) = current model time, yr
!!! pWriteFlag = integer flags indicating which variables to write
!!! lH, fH (in) = low-, high-res ice thickness grids
!!! lT, fT (in) = low-, high-res bedrock topography grids
!!! lHT, fHT = low-, high-res ice surface topography grids
!!! lTempS, lTempB, lTempM (in) =  low-res ice surface, basal, and melting temperatures, C
!!! lUDefm, lVDefm (in) = x- and y-direction ice deformation velocity components
!!! lUSlid, lVSlid (in) = x- and y-direction ice sliding velocity components
!!! lSliding, fSliding (in) = low- and high-res ice sliding velocity magnitude
!!! lConstrict (in) = low-res constriction factor, nondim
!!! fGlacErosRate (in) = high-res glacial erosion rate, m/yr
!!! fhillErosRate (in) = high-res hillslope erosion rate, m/yr
!!! fSlope (in) = high-res surface slope, nondim
!!! fQWater (in) = high-res water flux, m^3/yr
!!! fFluvErosRate (in) = high-res fluvial erosion rate, m/yr
!!! fWater (in) = water input rate, m^3/yr
!!! dvolIceSrc, dvolIceSnk, dvolIce = change in ice volume (sources, sinks, net) over the main timestep
!!! lBalRate (in) = ice balance rate, m_ice/yr
!!! fLake (in) = logical flags for lake nodes
!!! fCatchment (in) = integer labels for fluvial catchments
!!! timeStepIceMean = mean ice time step over the larger main model timestep
!!! tempSl (in) = current sea-level temperature, C
!!! fFlex (in) = flexural isostatic deflection, m
!!! fSolnH (in) = time-dependant benchmark solution for ice thickness
!
!logical, intent(in) :: fLake(:,:)
!integer, intent(in) :: step, fCatchment(:,:), pWriteFlag(:)
!real (dp), intent(in) :: time,  lH(:,:), fH(:,:), lT(:,:), fT(:,:), lHT(:,:), fHT(:,:), &
!	lTempS(:,:), lTempB(:,:), lTempM(:,:), lUDefm(:,:), lVDefm(:,:), lUSlid(:,:), lVSlid(:,:), &
!	lSliding(:,:), lConstrict(:,:), fSliding(:,:), fGlacErosRate(:,:), fhillErosRate(:,:), &
!	fSlope(:,:), fQWater(:,:), fFluvErosRate(:,:), fWater(:,:), dvolIceSrc, dvolIceSnk, dvolIce, &
!	lBalRate(:,:), timeStepIceMean, tempSl, fFlex(:,:), fSolnH(:,:)
!	
!integer :: msg, lNx, lNy
!
!! Get low-res grid dimensions
!lNx = size(lH,1)
!lNy = size(lH,2)
!
!! Write summary results to screen 
!print*,'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
!print*,'time = ',time 
!print*,'fT min,mean,max', nint(minval(fT)), nint(sum(fT)/size(fT)), nint(maxval(fT))
!print*,'lH min,mean,max', nint(minval(lH(2:lNx-1,2:lNy-1))), &
!	nint(sum(lH(2:lNx-1,2:lNy-1))/(lNx-2)/(lNy-2)), nint(maxval(lH(2:lNx-1,2:lNy-1)))
!print*,'timeStepIceMean',real(timeStepIceMean,sp)
!print*,'fQWater min,mean,max', real(minval(fQWater),sp), real(sum(fQWater)/size(fQWater),sp), &
!	real(maxval(fQWater),sp)
!
!
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
