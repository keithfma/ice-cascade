! =============================================================================
! Input/Output routines for the ice-cascade landscape evolution model.
!
! Contains:
!   readParams (public): read from the input file specified on the command line
!   
! ============================================================================

module io

! Module contains...
!! initGrids: Reads or creates the initial model state (topography & ice thickness)
!! createOutput: creates the output netcdf file
!! writeConst: writes constant variables to the output file, once
!! writeStep: writes the values of time-dependent variables at this step to the output netcdf file
!! closeOutput: close the output file.
!! debugOut2d: Writes a single 2D array to file, used for debugging purposes

use types, only: sp, dp, sp_nc, dp_nc, int_nc
use grid_module, only: grid_type
use hill_module, only: hill_type
use netcdf, only: nf90_open, nf90_nowrite, nf90_inquire, nf90_inquire_dimension, &
	nf90_inquire_variable, nf90_get_var, nf90_netcdf4, nf90_create, nf90_def_dim, nf90_unlimited, &
	nf90_global, nf90_put_att, nf90_def_var, nf90_enddef, nf90_put_var, nf90_close, nf90_double, &
	nf90_def_var

implicit none
private
public :: readParams, initGrids, createOutput, writeConst, writeStep, closeOutput, debugOut2d

! netcdf handles
!! save handles between function calls
integer, save :: oFile, olXDim, olYDim, ofXDim, ofYDim, oTimeDim, olX, olY, ofX, ofY, oTime, olH, ofH, &
	olT, ofT, olHT, ofHT, olTempS, olTempB, olTempM, olUDefm, olVDefm, olUSlid, olVSlid, &
	olSliding, ofSliding, olConstrict, ofUpliftRate, ofGlacErosRate, ofHillErosRate, ofSlope, &
	ofQWater, ofFluvErosRate, ofWater, oDvolIceSrc, oDvolIceSnk, oDvolIce, ofSolnHss, &
	olBalRate, ofLake, ofCatchment, oTimeStepIceMean, oTempSl, ofFlex, ofSolnH

contains

  ! ---------------------------------------------------------------------------
  ! SUB: read from the input file specified on the command line
  ! ---------------------------------------------------------------------------
  subroutine readParams (fGrid, fHill)
    
    type(grid_type), intent(out) :: fGrid ! high-res grid
    type(hill_type), intent(out) :: fHill ! hilllslope model

    character(len=100) :: infile, line
    integer :: tmp, msg

    ! TEMPORARY
    character(len=150) :: pRunName, pTopoFile
    real(dp) :: pTimeEnd, pTimeStep
    integer :: pWriteFreq
  	
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
    read (55,*) pRunName	
    read (55,*) pTimeEnd	
    read (55,*) pTimeStep
    read (55,*) pWriteFreq
    read (55,*) fGrid%nx	
    read (55,*) fGrid%ny
    read (55,*) fGrid%dx
    read (55,*) fGrid%dy	
    read (55,*) pTopoFile
    read (55,*) tmp; fHill%on = merge(.true., .false., tmp==1)
    read (55,*) fHill%D	
    read (55,*) fHill%nbcName
    read (55,*) fHill%sbcName 
    read (55,*) fHill%ebcName
    read (55,*) fHill%wbcName 
    read (55,*) fHill%solnName 	
    read (55,*) tmp; fHill%writeDzdt = merge(.true., .false., tmp==1)
    close(55)
    
  end subroutine readParams

! ==================================================================================================
! initGrids: Reads or creates the initial model state (topography & ice thickness)
! ==================================================================================================
subroutine initGrids ( pTopoFile, pIceFile, pBenchmark, lT, fT, lH, fH, lDx, lDy, fDx, fDy, lX, &
	fX, lY, fY, pDoAddNoise, pDoPrefilter, pUpliftRate, pB, pRhoIce )

! Arguments:
!! pTopoFile (in) = name of the netcdf file containing the initial topography
!! pIceFile (in) = name of the netcdf file containing the intial ice thickness
!! pBenchmark (in) = integer flag indicating whether a benchmark is to do run, and which
!! lT, fT (out) = low-, high-res initial topography grid
!! lH, fH (out) = low-, high-res initial ice thickness grid
!! lDx, lDy (in)  = low-res grid spacing, m
!! fDx, fDy (in)  = high-res grid spacing, m
!! lX, lY (out) = low-res coordinate grids, m
!! fX, fY (out) = high-res coordinate grids, m
!! pDoAddNoise (in) = logical flag, add noise to initial topography?
!! pDoPrefilter (in) = logical flag, apply smoothing filter to initial topography?
!! pB (in) = ice deformation rate constant
!! pRhoIce(in) = ice density

character(len=*), intent(in) :: pTopoFile, pIceFile
logical, intent(in) :: pDoAddNoise, pDoPrefilter
integer, intent(in) :: pBenchmark
real(dp), intent(in) :: lDx, lDy, fDx, fDy, pUpliftRate, pB, pRhoIce
real(dp), intent(out) :: lT(:,:), fT(:,:), lH(:,:), fH(:,:), lX(:,:), lY(:,:), fX(:,:), fY(:,:)

integer :: i, j, k, msg, iTopoFile, numDim, numVar, n1, n2, ifT, iIceFile, ifH,  lNx, fNx, lNy, fNy
real(dp) :: noise

! Get grid sizes
lNx = size(lX,1)
lNy = size(lX,2)
fNx = size(fX,1)
fNy = size(fX,2)

! Make coordinate grids
!! High-res
do j = 1,fNy
	do i = 1,fNx
		fX(i,j) = float(i-1)*fDx
		fY(i,j) = float(j-1)*fDy
	enddo
enddo
!! Low-res
do j = 1,lNy
	do i = 1,lNx
		!! offset by 1 point to account for bc points
		lX(i,j) = -lDx+float(i-1)*lDx
		lY(i,j) = -lDy+float(j-1)*lDy
	enddo
enddo

! Normal model, read from file
if (pBenchmark==0) then

	!! Read initial topography
	if (pTopoFile=='zero') then
		fT = 0._dp
	else
		!!! Open file
		msg = nf90_open( trim(pTopoFile), nf90_nowrite, iTopoFile )
		msg = nf90_inquire( iTopoFile, numDim, numVar )	
		!!! Confirm the dimensions
		if (numDim/=2) then
			print*,'Bad input file, should have only two dimensions'
		else
			msg = nf90_inquire_dimension(iTopoFile, 1, len = n1)	
			if (n1/=fNx) print*,'Input data does not match the given dimensions'
			msg = nf90_inquire_dimension(iTopoFile, 2, len = n2)
			if (n2/=fNy) print*,'Input data does not match the given dimensions'
		end if	
		!!! Find the variable containing topography, must be the only 2d variable
		ifT = -1
		do i = 1,numVar
			msg = nf90_inquire_variable(iTopoFile, i, ndims=numDim)
			if (numDim==2) then
				if (ifT/=-1) then
					print*,'Bad input file, only one var should be 2-dimensional'
					return
				else
					ifT = i
				end if
			end if 
		end do	
		!!! Read in the topography
		msg = nf90_get_var(iTopoFile, ifT, fT)
	endif
		
	!! Additional treatment of initial topography
	!!! Add random noise (optional) 
	call random_seed()
	if (pDoAddNoise) then
		do j = 1,fNy
			do i = 1,fNx
				call random_number(noise)
				!!!! Note: 10e3 * maximum uplift rate is needed so that uplift does not overwhelm 
				!!!!! the nascent drainage network, yielding unnatural smooth planar surfaces.
				fT(i,j) = fT(i,j) + noise*max(1._dp, pUpliftRate*10.e3_dp)
			enddo
		enddo
	end if
	!! Prefilter the high-res grid (optional) 
	if (pDoPrefilter) then      
		do k = 1,2
			do i = 2,fNx-1
				do j = 2,fNy-1
					fT(i,j) = (8._dp*fT(i,j)+2._dp*fT(i-1,j)+2._dp*fT(i+1,j)+2._dp*fT(i,j-1)+ &
						2._dp*fT(i,j+1)+fT(i-1,j-1)+fT(i-1,j+1)+fT(i+1,j-1)+fT(i+1,j+1))/20._dp
				enddo
			enddo
		enddo
	endif
	
	!! Read initial ice thickness
	if (pIceFile=='zero') then
		fH = 0._dp
	else	
		!!! Open file
		msg = nf90_open( trim(pIceFile), nf90_nowrite, iIceFile )
		msg = nf90_inquire( iIceFile, numDim, numVar )	
		!!! Confirm the dimensions
		if (numDim/=2) then
			print*,'Bad input file, should have only two dimensions'
		else
			msg = nf90_inquire_dimension(iIceFile, 1, len = n1)	
			if (n1/=fNx) print*,'Input data does not match the given dimensions'
			msg = nf90_inquire_dimension(iIceFile, 2, len = n2)
			if (n2/=fNy) print*,'Input data does not match the given dimensions'
		end if	
		!!! Find the variable containing topography, must be the only 2d variable
		ifH = -1
		do i = 1,numVar
			msg = nf90_inquire_variable(iIceFile, i, ndims=numDim)
			if (numDim==2) then
				if (ifH/=-1) then
					print*,'Bad input file, only one var should be 2-dimensional'
					return
				else
					ifH = i
				end if
			end if 
		end do	
		!!! Read in the ice thickness
		msg = nf90_get_var(iIceFile, ifH, fH)
		
	end if

! Benchmark cases
!else
!	call benchInitGrids( pBenchmark, fX, fY, fT, fH, pB, pRhoIce )
	
end if

!! Populate low-res grids 
!call highToLowRes ( lX, fX, lY, fY, fDx, fDy, lT, fT )
!call highToLowRes ( lX, fX, lY, fY, fDx, fDy, lH, fH )

end subroutine initGrids

! ==================================================================================================
! createOutput: creates the output netcdf file
! ==================================================================================================
subroutine createOutput ( pRunName, pTopoFile, pIceFile, lNx, fNx, lNy, fNy, lDx, fDx, lDy, fDy, &
	pDoEros, pDoFlex, pDoTemp, pTimeEnd, pTimeStepIceMax, pTimeStepIceMin, pTimeStep, pB, pBs, &
	pGamma, pQheatb, pHeatConduct, pTempSlMin, pTempSlMax, pTempPeriod, pTempLapse, pTempYrRng, &
	pMeltFact, pPrecipRate, pRhoIce, pRhoWater, pRhoCrust, pRhoMantle, pYm, pNu, pTe, &
	pGlacErosFact, pHillD, pUpliftRate, pGlacErosRateCeil, pTFloor, pGlacBcN, pGlacBcS, pGlacBcE, &
	pGlacBcW, pDoGlac, pDoFluv, pDoHill, pDoUplift, pDoTrackIceVol, pDoAddNoise, pDoPrefilter, &
	pFluvBcN, pFluvBcS, pFluvBcE, pFluvBcW, pFluvErosFact, pBaseLvl, pNxPad, pNyPad, pHillBcN, &
	pHillBcS, pHillBcE, pHillBcW, pWriteFlag )
	
! Arguments:
!! pRunName (out) = string containing the run name
!! pTopoFile (out) = filename of the netcdf containing the initial topography, or "zero"
!! pIceFile (out) = filename of the netcdf containing the initial ice thickness, or "zero"
!! lNx, lNy (out) = low-res grid dimensions
!! fNx, fNy (out) = high-res grid dimensions
!! lDx, lDy (out) = low-res grid spacing, m
!! fDx, fDy (out) = high-res grid spacing, m
!! pDoEros (out) = logical flag, erode?
!! pDoFlex (out) = logical flag, compute flexure?
!! pDoTemp (out) = logical flag, polythermal ice?
!! pTimeEnd (out) = model end time, yr
!! pTimeStepIceMax (out) = maximum time step for the ice model, yr
!! pTimeStepIceMin (out) = minimum time step for the ice model, yr
!! pTimeStep (out) = model main time step, yr
!! pB (out) = ice-deformation constant (B), Pa^-3 a^-1
!! pBs (out) = sliding law constant (Bs), Pa^-3 a^-1  m^-2
!! pGamma (out) = constriction factor constant, m
!! pQheatb (out) = basal heat flux, W m^-2
!! pHeatConduct (out) = ice conductivity, W m^-1 K^-1
!! pTempSlMin, pTempSlMax (out) = minimum, maximum sea-level temperature over a climate cycle, C
!! pTempPeriod (out) = period of the climate cycle, yr
!! pTempLapse (out) = atmospheric temperature lapse rate, C/m
!! pTempYrRng (out) = yearly temperature range, C
!! pMeltFact (out) = positive-degree-day melting factor, m_ice/degC/day
!! pPrecipRate (out) = precip rate, m/yr
!! pRhoIce, pRhoWater, pRhoCrust, pRhoMantle  (out) = densities, kg/m^3
!! pYm (out) = Young's modulus, Pa
!! pNu (out) = Poisson's ratio, nondim
!! pTe (out) = elastic thickness, m
!! pGlacErosFact (out) = glacial erosion rate constant, nondim
!! pHillD (out) = hillslope diffusivity, m^2/yr
!! pUpliftRate (out) = tectonic uplift rate, m/yr
!! pGlacErosRateCeil (out) = maximum allowed glacial erosion rate, m/yr
!! pTFloor (out) = minimum allowed elevation, m
!! pGlacBcN, pGlacBcS, pGlacBcE, pGlacBcW (out) = ice model boundary conditions 
!! pDoGlac (out) = logical flag, run ice model?
!! pDoFluv (out) = logical flag, run fluvial model?
!! pDoHill (out) = logical flag, run hillslope model?
!! pDoUplift (out) = logical flag, run uplift?
!! pDoTrackIceVol (out) = logical flag, track ice volume?
!! pDoAddNoise (out) = logical flag, add noise to initial topography?
!! pDoPrefilter (out) = logical flag, apply smoothing filter to initial topography?
!! pFluvBcN, pFluvBcS, pFluvBcE, pFluvBcW (out) = fluvial model boundary conditions
!! pFluvErosFact (out) = fluvial erosion rate constant
!! pBenchmark (out) = integer label indicating if the model is a benchmark (\=0) and if so, which
!! pWriteFreq (out) = frequency of output, num main time steps
!! pBaseLvl (out) = base level for the fluvial model, m
!! pNxPad, pNyPad (out) = grid dimensions for the padded grid used in the flexure fourier transform
!! pWriteFlag = integer flags for output variables (1/0)
	
character(len=150), intent(in) :: pRunName, pTopoFile, pIceFile
logical, intent(in) :: pDoTemp, pDoEros, pDoFlex, pDoGlac, pDoFluv, pDoHill, pDoUplift, &
	pDoTrackIceVol, pDoAddNoise, pDoPrefilter
integer, intent(in) :: lNx, lNy, fNx, fNy, pGlacBcN, pGlacBcS, pGlacBcE, pGlacBcW, pWriteFlag(:), &
	pFluvBcN, pFluvBcS, pFluvBcE, pFluvBcW, pNxPad, pNyPad, pHillBcN, pHillBcS, pHillBcE, pHillBcW
real (dp), intent(in) :: pTimeStepIceMax, pTimeStepIceMin, pTimeStep, pTimeEnd, pB, pBs, pGamma, pTempYrRng, &
	pMeltFact, pTempLapse, pQheatb, pHeatConduct, pUpliftRate, pGlacErosFact, pHillD, pTempPeriod, &
	pTempSlMax, pTempSlMin, lDx, lDy, fDx, fDy, pGlacErosRateCeil, pTFloor, pPrecipRate, pRhoIce, &
	pRhoWater, pRhoCrust, pRhoMantle, pYm, pNu, pTe, pFluvErosFact, pBaseLvl

logical :: shuf
integer :: msg, i, j, deflateLevel
integer, dimension(3) :: lChunk, fChunk
 
! Define compression and chunking parameters 
deflateLevel = 1 ! compression, 0 = none, 9 = max, best value is 1
lChunk = (/ lNx-2, lNy-2, 1 /)
fChunk = (/ fNx-2, fNy-2, 1 /)
shuf = .true.

! Create output netcdf file

!! Open
msg = nf90_create( trim(pRunName)//'.out', nf90_netcdf4, oFile ) ! NETCDF-4 format, allows compression

!! Define dimensions
msg = nf90_def_dim( oFile, 'lX', lNx-2, olXDim ) 
msg = nf90_def_dim( oFile, 'lY', lNy-2, olYDim ) 
msg = nf90_def_dim( oFile, 'fX', fNx, ofXDim ) 
msg = nf90_def_dim( oFile, 'fY', fNy, ofYDim ) 
msg = nf90_def_dim( oFile, 'time', nf90_unlimited, oTimeDim )

!! Write parameters as global attributes
msg = nf90_put_att( oFile, nf90_global, 'glacial model flag', merge(1,0,pDoGlac))
msg = nf90_put_att( oFile, nf90_global, 'fluvial model flag', merge(1,0,pDoFluv))
msg = nf90_put_att( oFile, nf90_global, 'hillslope model flag', merge(1,0,pDoHill))
msg = nf90_put_att( oFile, nf90_global, 'isostasy model flag', merge(1,0,pDoFlex))
msg = nf90_put_att( oFile, nf90_global, 'erosion flag', merge(1,0,pDoEros))
msg = nf90_put_att( oFile, nf90_global, 'uplift flag', merge(1,0,pDoUplift))
msg = nf90_put_att( oFile, nf90_global, 'add 1m noise to initial topo', merge(1,0,pDoAddNoise))
msg = nf90_put_att( oFile, nf90_global, 'apply smoothing filter to initial topo', merge(1,0,pDoPrefilter))
msg = nf90_put_att( oFile, nf90_global, 'input topography file', pTopoFile)
msg = nf90_put_att( oFile, nf90_global, 'input ice thickness file', pIceFile)	
msg = nf90_put_att( oFile, nf90_global, 'maximum timestep for the ice model, yrs', pTimeStepIceMax)
msg = nf90_put_att( oFile, nf90_global, 'minimum timestep for the ice model, yrs', pTimeStepIceMin)
msg = nf90_put_att( oFile, nf90_global, 'main timestep (not for ice model), yrs', pTimeStep)
msg = nf90_put_att( oFile, nf90_global, 'model end time, yrs', pTimeEnd)
msg = nf90_put_att( oFile, nf90_global, 'basal temperature calculation flag', merge(1,0,pDoTemp))
msg = nf90_put_att( oFile, nf90_global, 'ice volume tracking flag', merge(1,0,pDoTrackIceVol))
msg = nf90_put_att( oFile, nf90_global, 'ice deformation coefficient, Pa^-3 s^-1', pB)
msg = nf90_put_att( oFile, nf90_global, 'ice deformation exponent, nondim', 3.)
msg = nf90_put_att( oFile, nf90_global, 'basal heat flux, W m^-2', pQheatb)
msg = nf90_put_att( oFile, nf90_global, 'ice thermal conductivity, W m^-1 K-1', pHeatConduct) 
msg = nf90_put_att( oFile, nf90_global, 'ice boundary condition, north', pGlacBcN )
msg = nf90_put_att( oFile, nf90_global, 'ice boundary condition, south', pGlacBcS )
msg = nf90_put_att( oFile, nf90_global, 'ice boundary condition, east', pGlacBcE )
msg = nf90_put_att( oFile, nf90_global, 'ice boundary condition, west', pGlacBcW )
msg = nf90_put_att( oFile, nf90_global, 'ice density, kg m^-3', pRhoIce)
msg = nf90_put_att( oFile, nf90_global, 'ice sliding coefficient, m^2 Pa^-3 a^-1', pBs)
msg = nf90_put_att( oFile, nf90_global, 'ice sliding exponent, nondim', 3._dp)
msg = nf90_put_att( oFile, nf90_global, 'ice constriction coefficient, m', pGamma)
msg = nf90_put_att( oFile, nf90_global, 'glacial erosion coefficient, nondim', pGlacErosFact)
msg = nf90_put_att( oFile, nf90_global, 'maximum allowed glacial erosion rate, m yr^-1', pGlacErosRateCeil )
msg = nf90_put_att( oFile, nf90_global, 'fluvial erosion rate constant, (m^3 yr^-1)^0.5', pFluvErosFact)
msg = nf90_put_att( oFile, nf90_global, 'fluvial base-level, (m)', pBaseLvl)
msg = nf90_put_att( oFile, nf90_global, 'fluvial boundary condition, north', pFluvBcN )
msg = nf90_put_att( oFile, nf90_global, 'fluvial boundary condition, south', pFluvBcS )
msg = nf90_put_att( oFile, nf90_global, 'fluvial boundary condition, east', pFluvBcE )
msg = nf90_put_att( oFile, nf90_global, 'fluvial boundary condition, west', pFluvBcW )
msg = nf90_put_att( oFile, nf90_global, 'hillslope diffusivity, m^2 a^-1', pHillD)
msg = nf90_put_att( oFile, nf90_global, 'hillslope boundary condition, north', pHillBcN )
msg = nf90_put_att( oFile, nf90_global, 'hillslope boundary condition, south', pHillBcS )
msg = nf90_put_att( oFile, nf90_global, 'hillslope boundary condition, east', pHillBcE )
msg = nf90_put_att( oFile, nf90_global, 'hillslope boundary condition, west', pHillBcW )
msg = nf90_put_att( oFile, nf90_global, 'youngs modulus', pYm)
msg = nf90_put_att( oFile, nf90_global, 'poissons ratio', pNu)
msg = nf90_put_att( oFile, nf90_global, 'elastic thickness of the lithospere', pTe)
msg = nf90_put_att( oFile, nf90_global, 'dimensions of padded grid for flexure, x', pNxPad)
msg = nf90_put_att( oFile, nf90_global, 'dimensions of padded grid for flexure, y', pNyPad)
msg = nf90_put_att( oFile, nf90_global, 'tectonic rock uplift rate, m a^-1', pUpliftRate)
msg = nf90_put_att( oFile, nf90_global, 'minimum allowed bedrock elevation, m', pTFloor)	
msg = nf90_put_att( oFile, nf90_global, 'minimum sea-level surface temperature, C', pTempSlMin)
msg = nf90_put_att( oFile, nf90_global, 'maximum sea-level surface temperature, C', pTempSlMax)
msg = nf90_put_att( oFile, nf90_global, 'period of surface temperature oscillation, yrs', pTempPeriod)
msg = nf90_put_att( oFile, nf90_global, 'surface temperature lapse rate, K m^-1', pTempLapse)
msg = nf90_put_att( oFile, nf90_global, 'amplitude of annual temperature cycle, deg. C', pTempYrRng)
msg = nf90_put_att( oFile, nf90_global, 'melting rate constant, m(ice)/day/deg. C ', pMeltFact) 
msg = nf90_put_att( oFile, nf90_global, 'precipitation rate, m a^-1', pPrecipRate)
msg = nf90_put_att( oFile, nf90_global, 'water density, kg m^-3', pRhoWater)
msg = nf90_put_att( oFile, nf90_global, 'crust density, kg m^-3', pRhoCrust)
msg = nf90_put_att( oFile, nf90_global, 'mantle density, kg m^-3', pRhoMantle)

!! Create variables
msg = nf90_def_var( oFile, 'time', sp_nc, oTimeDim, oTime )
msg = nf90_put_att( oFile, oTime, 'long_name', 'model time')
msg = nf90_put_att( oFile, oTime, 'units', 'years')

msg = nf90_def_var( oFile, 'lX', sp_nc, olXDim, olX )
msg = nf90_put_att( oFile, olX, 'long_name', 'x-position, low resolution grid')
msg = nf90_put_att( oFile, olX, 'units', 'm')

msg = nf90_def_var( oFile, 'lY', sp_nc, olYDim, olY )
msg = nf90_put_att( oFile, olY, 'long_name', 'y-position, low resolution grid')
msg = nf90_put_att( oFile, olY, 'units', 'm')

msg = nf90_def_var( oFile, 'fX', sp_nc, ofXDim, ofX )
msg = nf90_put_att( oFile, ofX, 'long_name', 'x-position, high resolution grid')
msg = nf90_put_att( oFile, ofX, 'units', 'm')

msg = nf90_def_var( oFile, 'fY', sp_nc, ofYDim, ofY )
msg = nf90_put_att( oFile, ofY, 'long_name', 'y-position, high resolution grid')
msg = nf90_put_att( oFile, ofY, 'units', 'm')

if (pWriteFlag(1)==1) then
	msg = nf90_def_var( oFile, 'lH', sp_nc, (/ olXDim, olYDim, oTimeDim /), olH, &
		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, olH, 'long_name', 'ice thickness, low resolution grid')
	msg = nf90_put_att( oFile, olH, 'units', 'm')
	msg = nf90_put_att( oFile, olH, 'valid_min', 0._dp)
end if	

if (pWriteFlag(2)==1) then
	msg = nf90_def_var( oFile, 'fH', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofH, &
		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofH, 'long_name', 'ice thickness, high resolution grid')
	msg = nf90_put_att( oFile, ofH, 'units', 'm')
	msg = nf90_put_att( oFile, ofH, 'valid_min', 0._dp)
end if	
	
if (pWriteFlag(3)==1) then	
	msg = nf90_def_var( oFile, 'lT', sp_nc, (/ olXDim, olYDim, oTimeDim /), olT, &
		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, olT, 'long_name', 'bedrock elevation, low resolution grid')
	msg = nf90_put_att( oFile, olT, 'units', 'm')
end if

if (pWriteFlag(4)==1) then	
	msg = nf90_def_var( oFile, 'fT', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofT, &
		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofT, 'long_name', 'bedrock elevation, high resolution grid')
	msg = nf90_put_att( oFile, ofT, 'units', 'm')
end if

if (pWriteFlag(5)==1) then
	msg = nf90_def_var( oFile, 'lHT', sp_nc, (/ olXDim, olYDim, oTimeDim /), olHT, &
		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, olHT, 'long_name', 'ice surface topography, low resolution grid')
	msg = nf90_put_att( oFile, olHT, 'units', 'm')
end if	

if (pWriteFlag(6)==1) then
	msg = nf90_def_var( oFile, 'fHT', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofHT, &
		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofHT, 'long_name', 'ice surface topography, high resolution grid')
	msg = nf90_put_att( oFile, ofHT, 'units', 'm')
end if	

if (pWriteFlag(7)==1) then
	msg = nf90_def_var( oFile, 'lTempS', sp_nc, (/ olXDim, olYDim, oTimeDim /), olTempS, &
		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, olTempS, 'long_name', 'surface temperature, low resolution grid')
	msg = nf90_put_att( oFile, olTempS, 'units', 'degrees C')
end if

if (pWriteFlag(8)==1) then
	msg = nf90_def_var( oFile, 'lTempB', sp_nc, (/ olXDim, olYDim, oTimeDim /), olTempB, &
		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, olTempB, 'long_name', 'basal temperature, low resolution grid')
	msg = nf90_put_att( oFile, olTempB, 'units', 'degrees C')
end if

if (pWriteFlag(9)==1) then
	msg = nf90_def_var( oFile, 'lTempM', sp_nc, (/ olXDim, olYDim, oTimeDim /), olTempM, &
		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, olTempM, 'long_name', &
		'ice melting temperature, low resolution grid')
	msg = nf90_put_att( oFile, olTempM, 'units', 'degrees C')	
end if

if (pWriteFlag(10)==1) then
	msg = nf90_def_var( oFile, 'lUDefm', sp_nc, (/ olXDim, olYDim, oTimeDim /), olUDefm, &
		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, olUDefm, 'long_name', 'ice deformation velocity, x-component, low resolution grid')
	msg = nf90_put_att( oFile, olUDefm, 'units', 'm/yr')	
end if

if (pWriteFlag(11)==1) then
	msg = nf90_def_var( oFile, 'lVDefm', sp_nc, (/ olXDim, olYDim, oTimeDim /), olVDefm, &
		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, olUDefm, 'long_name', 'ice deformation velocity, y-component, low resolution grid')
	msg = nf90_put_att( oFile, olVDefm, 'units', 'm/yr')	
end if

if (pWriteFlag(12)==1) then
	msg = nf90_def_var( oFile, 'lUSlid', sp_nc, (/ olXDim, olYDim, oTimeDim /), olUSlid, &
		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, olUSlid, 'long_name', 'ice sliding velocity, x-component, low resolution grid')
	msg = nf90_put_att( oFile, olUSlid, 'units', 'm/yr')	
end if

if (pWriteFlag(13)==1) then
	msg = nf90_def_var( oFile, 'lVSlid', sp_nc, (/ olXDim, olYDim, oTimeDim /), olVSlid, &
		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, olVSlid, 'long_name', 'ice sliding velocity, y-component, low resolution grid')
	msg = nf90_put_att( oFile, olVSlid, 'units', 'm/yr')	
end if

if (pWriteFlag(14)==1) then
	msg = nf90_def_var( oFile, 'lSliding', sp_nc, (/ olXDim, olYDim, oTimeDim /), olSliding, &
		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, olSliding, 'long_name', 'ice sliding velocity magnitude, low resolution grid')
	msg = nf90_put_att( oFile, olSliding, 'units', 'm/yr')	
end if

if (pWriteFlag(15)==1) then	
	msg = nf90_def_var( oFile, 'fSliding', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofSliding, &
		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofSliding, 'long_name', 'ice sliding velocity magnitude, high resolution grid')
	msg = nf90_put_att( oFile, ofSliding, 'units', 'm/yr')
end if

if (pWriteFlag(16)==1) then
	msg = nf90_def_var( oFile, 'lConstrict', sp_nc, (/ olXDim, olYDim, oTimeDim /), olConstrict, &
		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, olConstrict, 'long_name', 'ice constriction factor, low resolution grid')
	msg = nf90_put_att( oFile, olConstrict, 'units', 'non-dimensional')	
end if

if (pWriteFlag(17)==1) then	
	msg = nf90_def_var( oFile, 'fUpliftRate', sp_nc, (/ ofXDim, ofYDim /), ofUpliftRate, &
		chunksizes = (/ fNx, fNy /), shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofUpliftRate, 'long_name', 'tectonic uplift rate,  high resolution grid')
	msg = nf90_put_att( oFile, ofUpliftRate, 'units', 'm/yr')
end if

if (pWriteFlag(18)==1) then	
	msg = nf90_def_var( oFile, 'fGlacErosRate', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofGlacErosRate, &
		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofGlacErosRate, 'long_name', 'glacial erosion rate,  high resolution grid')
	msg = nf90_put_att( oFile, ofGlacErosRate, 'units', 'm/yr')
end if

if (pWriteFlag(19)==1) then	
	msg = nf90_def_var( oFile, 'fHillErosRate', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofHillErosRate, &
		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofHillErosRate, 'long_name', 'hillslope erosion rate,  high resolution grid')
	msg = nf90_put_att( oFile, ofHillErosRate, 'units', 'm/yr')
end if

if (pWriteFlag(20)==1) then	
	msg = nf90_def_var( oFile, 'fSlope', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofSlope, &
		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofSlope, 'long_name', 'bedrock slope,  high resolution grid')
	msg = nf90_put_att( oFile, ofSlope, 'units', 'nondim')
end if

if (pWriteFlag(21)==1) then	
	msg = nf90_def_var( oFile, 'fQWater', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofQWater, &
		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofQWater, 'long_name', 'water discharge, high resolution grid')
	msg = nf90_put_att( oFile, ofQWater, 'units', 'm^3/yr')
end if

if (pWriteFlag(22)==1) then	
	msg = nf90_def_var( oFile, 'fFluvErosRate', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofFluvErosRate, &
		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofFluvErosRate, 'long_name', 'fluvial erosion rate,  high resolution grid')
	msg = nf90_put_att( oFile, ofFluvErosRate, 'units', 'm/yr')
end if

if (pWriteFlag(23)==1) then	
	msg = nf90_def_var( oFile, 'fWater', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofWater, &
		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofWater, 'long_name', 'water input rate (rainfall + ice melt),  high resolution grid')
	msg = nf90_put_att( oFile, ofWater, 'units', 'm/yr')
end if

if (pWriteFlag(24)==1) then
	
	msg = nf90_def_var( oFile, 'dvolIceSrc', sp_nc, (/ oTimeDim /), oDvolIceSrc )
	msg = nf90_put_att( oFile, oDvolIceSrc, 'long_name', &
		'ice volume added by mass balance and flux at domain boudaries for each step,  scalar')
	msg = nf90_put_att( oFile, oDvolIceSrc, 'units', 'm^3')
	
	msg = nf90_def_var( oFile, 'dvolIceSnk', sp_nc, (/ oTimeDim /), oDvolIceSnk )
	msg = nf90_put_att( oFile, oDvolIceSnk, 'long_name', &
		'ice volume lost by mass balance and flux at domain boudaries for each step, scalar')
	msg = nf90_put_att( oFile, oDvolIceSnk, 'units', 'm^3')

	msg = nf90_def_var( oFile, 'dvolIce', sp_nc, (/ oTimeDim /), oDvolIce )
	msg = nf90_put_att( oFile, oDvolIce, 'long_name', 'change in ice volume for each step, scalar')
	msg = nf90_put_att( oFile, oDvolIce, 'units', 'm^3')

end if

!! Note: dropped pWriteFlag(25), this variable was used for lHSoln

if (pWriteFlag(26)==1) then
	msg = nf90_def_var( oFile, 'fSolnHss', sp_nc, (/ ofXDim, ofYDim /), ofSolnHss, &
		chunksizes = (/ fNx, fNy /), shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofSolnHss, 'long_name', &
		'Exact solution for ice thickness at steady state,  high resolution grid')
	msg = nf90_put_att( oFile, ofSolnHss, 'units', 'm')
end if

if (pWriteFlag(27)==1) then
	msg = nf90_def_var( oFile, 'lBalRate', sp_nc, (/ olXDim, olYDim, oTimeDim /), olBalRate, &
		chunksizes = lChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, olBalRate, 'long_name', 'balance rate, low resolution grid')
	msg = nf90_put_att( oFile, olBalRate, 'units', 'm ice / yr')
end if

if (pWriteFlag(28)==1) then
	msg = nf90_def_var( oFile, 'fLake', int_nc, (/ ofXDim, ofYDim, oTimeDim /), ofLake, &
		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofLake, 'long_name', 'lakes, high resolution grid')
	msg = nf90_put_att( oFile, ofLake, 'units', 'true/false')
end if

if (pWriteFlag(29)==1) then
	msg = nf90_def_var( oFile, 'fCatchment', int_nc, (/ ofXDim, ofYDim, oTimeDim /), ofCatchment, &
		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofCatchment, 'long_name', 'fluvial catchments, high resolution grid')
	msg = nf90_put_att( oFile, ofCatchment, 'units', 'unique integer flag')
end if

if (pWriteFlag(30)==1) then
	msg = nf90_def_var( oFile, 'timeStepIceMean', sp_nc, (/ oTimeDim /), oTimeStepIceMean )
	msg = nf90_put_att( oFile, oTimeStepIceMean, 'long_name','mean time step for the ice model')
	msg = nf90_put_att( oFile, oTimeStepIceMean, 'units', 'yr')
end if

if (pWriteFlag(31)==1) then
	msg = nf90_def_var( oFile, 'tempSl', sp_nc, (/ oTimeDim /), oTempSl )
	msg = nf90_put_att( oFile, oTempSl, 'long_name','sea level temperature, mean annual')
	msg = nf90_put_att( oFile, oTempSl, 'units', 'deg C')
end if

if (pWriteFlag(32)==1) then
	msg = nf90_def_var( oFile, 'fFlex', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofFlex, &
		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofFlex, 'long_name', 'flexural isostatic deflection, high resolution grid')
	msg = nf90_put_att( oFile, ofFlex, 'units', 'm')
end if

if (pWriteFlag(33)==1) then
	msg = nf90_def_var( oFile, 'fSolnH', sp_nc, (/ ofXDim, ofYDim, oTimeDim /), ofSolnH, &
		chunksizes = fChunk, shuffle = shuf, deflate_level = deflateLevel )
	msg = nf90_put_att( oFile, ofSolnH, 'long_name', &
		'Exact solution for ice thickness, time dependant, high resolution grid')
	msg = nf90_put_att( oFile, ofSolnH, 'units', 'm')
end if


!! Exit definition mode
msg = nf90_enddef( oFile ) 

!! Populate coordinates variables
do i = 1,lNx
	msg = nf90_put_var( oFile, olX, (i-1)*lDx, (/ i /) )
end do

do j = 1,lNy
	msg = nf90_put_var( oFile, olY, (j-1)*lDy, (/ j /) )
end do

do i = 1,fNx
	msg = nf90_put_var( oFile, ofX, (i-1)*fDx, (/ i /) )
end do

do j = 1,fNy
	msg = nf90_put_var( oFile, ofY, (j-1)*fDy, (/ j /) )
end do


return
end subroutine createOutput

! ==================================================================================================
! writeConst: writes constant variables to the output file, once
! ==================================================================================================
subroutine writeConst (fUpliftRate, fSolnHss, pWriteFlag)

! Arguments:
!! fUpliftRate (in) = tectonic uplift rate, m/yr
!! fSolnHss (in) = high-res exact solution for steady-state ice thickness in benchmark cases, m
!! pWriteFlag (in) = integer flags for which variables to write to output

integer, intent(in) :: pWriteFlag(:)
real(dp), intent(in) :: fUpliftRate(:,:), fSolnHss(:,:)
integer :: msg

if (pWriteFlag(17)==1) then
	msg = nf90_put_var(oFile, ofUpliftRate, real(fUpliftRate,sp), (/ 1, 1 /) )
end if

if (pWriteFlag(26)==1) then
	msg = nf90_put_var(oFile, ofSolnHss, real(fSolnHss,sp), (/ 1, 1 /) )
end if

return
end subroutine writeConst

! ==================================================================================================
! writeStep: writes the values of time-dependent variables at this step to the output netcdf file
! ==================================================================================================
subroutine writeStep ( step, time, pWriteFlag, lH, fH, lT, fT, lHT, fHT, lTempS, lTempB, lTempM, &
	lUDefm, lVDefm, lUSlid, lVSlid, lSliding, fSliding, lConstrict, fGlacErosRate, fHillErosRate, &
	fSlope, fQWater, fFluvErosRate, fWater, dvolIceSrc, dvolIceSnk, dvolIce, lBalRate, fLake, &
	fCatchment, timeStepIceMean, tempSl, fFlex, fSolnH )
	
! Arguments:
!! step (in) = number of the current output step
!! time (in) = current model time, yr
!! pWriteFlag = integer flags indicating which variables to write
!! lH, fH (in) = low-, high-res ice thickness grids
!! lT, fT (in) = low-, high-res bedrock topography grids
!! lHT, fHT = low-, high-res ice surface topography grids
!! lTempS, lTempB, lTempM (in) =  low-res ice surface, basal, and melting temperatures, C
!! lUDefm, lVDefm (in) = x- and y-direction ice deformation velocity components
!! lUSlid, lVSlid (in) = x- and y-direction ice sliding velocity components
!! lSliding, fSliding (in) = low- and high-res ice sliding velocity magnitude
!! lConstrict (in) = low-res constriction factor, nondim
!! fGlacErosRate (in) = high-res glacial erosion rate, m/yr
!! fHillErosRate (in) = high-res hillslope erosion rate, m/yr
!! fSlope (in) = high-res surface slope, nondim
!! fQWater (in) = high-res water flux, m^3/yr
!! fFluvErosRate (in) = high-res fluvial erosion rate, m/yr
!! fWater (in) = water input rate, m^3/yr
!! dvolIceSrc, dvolIceSnk, dvolIce = change in ice volume (sources, sinks, net) over the main timestep
!! lBalRate (in) = ice balance rate, m_ice/yr
!! fLake (in) = logical flags for lake nodes
!! fCatchment (in) = integer labels for fluvial catchments
!! timeStepIceMean = mean ice time step over the larger main model timestep
!! tempSl (in) = current sea-level temperature, C
!! fFlex (in) = flexural isostatic deflection, m
!! fSolnH (in) = time-dependant benchmark solution for ice thickness

logical, intent(in) :: fLake(:,:)
integer, intent(in) :: step, fCatchment(:,:), pWriteFlag(:)
real (dp), intent(in) :: time,  lH(:,:), fH(:,:), lT(:,:), fT(:,:), lHT(:,:), fHT(:,:), &
	lTempS(:,:), lTempB(:,:), lTempM(:,:), lUDefm(:,:), lVDefm(:,:), lUSlid(:,:), lVSlid(:,:), &
	lSliding(:,:), lConstrict(:,:), fSliding(:,:), fGlacErosRate(:,:), fHillErosRate(:,:), &
	fSlope(:,:), fQWater(:,:), fFluvErosRate(:,:), fWater(:,:), dvolIceSrc, dvolIceSnk, dvolIce, &
	lBalRate(:,:), timeStepIceMean, tempSl, fFlex(:,:), fSolnH(:,:)
	
integer :: msg, lNx, lNy

! Get low-res grid dimensions
lNx = size(lH,1)
lNy = size(lH,2)

! Write summary results to screen 
print*,'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
print*,'time = ',time 
print*,'fT min,mean,max', nint(minval(fT)), nint(sum(fT)/size(fT)), nint(maxval(fT))
print*,'lH min,mean,max', nint(minval(lH(2:lNx-1,2:lNy-1))), &
	nint(sum(lH(2:lNx-1,2:lNy-1))/(lNx-2)/(lNy-2)), nint(maxval(lH(2:lNx-1,2:lNy-1)))
print*,'timeStepIceMean',real(timeStepIceMean,sp)
print*,'fQWater min,mean,max', real(minval(fQWater),sp), real(sum(fQWater)/size(fQWater),sp), &
	real(maxval(fQWater),sp)


! Write output to netcdf 
msg = nf90_put_var(oFile, oTime, time, (/ step /) )

if (pWriteFlag(1)==1) then
	msg = nf90_put_var(oFile, olH, real(lH(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(2)==1) then
	msg = nf90_put_var(oFile, ofH, real(fH,sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(3)==1) then
	msg = nf90_put_var(oFile, olT, real(lT(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(4)==1) then
	msg = nf90_put_var(oFile, ofT, real(fT,sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(5)==1) then
	msg = nf90_put_var(oFile, olHT, real(lHT(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(6)==1) then
	msg = nf90_put_var(oFile, ofHT, real(fHT,sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(7)==1) then	
	msg = nf90_put_var(oFile, olTempS, real(lTempS(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )	
end if

if (pWriteFlag(8)==1) then
	msg = nf90_put_var(oFile, olTempB, real(lTempB(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(9)==1) then
	msg = nf90_put_var(oFile, olTempM, real(lTempM(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(10)==1) then
	msg = nf90_put_var(oFile, olUDefm, real(lUDefm(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(11)==1) then
	msg = nf90_put_var(oFile, olVDefm, real(lVDefm(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(12)==1) then
	msg = nf90_put_var(oFile, olUSlid, real(lUSlid(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(13)==1) then
	msg = nf90_put_var(oFile, olVSlid, real(lVSlid(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(14)==1) then
	msg = nf90_put_var(oFile, olSliding, real(lSliding(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(15)==1) then
	msg = nf90_put_var(oFile, ofSliding, real(fSliding,sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(16)==1) then
	msg = nf90_put_var(oFile, olConstrict, real(lConstrict(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(18)==1) then
	msg = nf90_put_var(oFile, ofGlacErosRate, real(fGlacErosRate,sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(19)==1) then
	msg = nf90_put_var(oFile, ofHillErosRate, real(fHillErosRate,sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(20)==1) then
	msg = nf90_put_var(oFile, ofSlope, real(fSlope,sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(21)==1) then
	msg = nf90_put_var(oFile, ofQWater, real(fQWater,sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(22)==1) then
	msg = nf90_put_var(oFile, ofFluvErosRate, real(fFluvErosRate,sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(23)==1) then
	msg = nf90_put_var(oFile, ofWater, real(fWater,sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(24)==1) then
	msg = nf90_put_var(oFile, oDvolIceSrc, real(dvolIceSrc,sp), (/ step /) )
	msg = nf90_put_var(oFile, oDvolIceSnk, real(dvolIceSnk,sp), (/ step /) )
	msg = nf90_put_var(oFile, oDvolIce, real(dvolIce,sp), (/ step /) )	
end if

if (pWriteFlag(27)==1) then
	msg = nf90_put_var(oFile, olBalRate, real(lBalRate(2:lNx-1,2:lNy-1),sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(28)==1) then
	msg = nf90_put_var(oFile, ofLake, merge(1, 0, fLake), (/ 1, 1, step /) )
end if

if (pWriteFlag(29)==1) then
	msg = nf90_put_var(oFile, ofCatchment, fCatchment, (/ 1, 1, step /) )
end if

if (pWriteFlag(30)==1) then
	msg = nf90_put_var(oFile, oTimeStepIceMean, timeStepIceMean, (/ step /) )
end if

if (pWriteFlag(31)==1) then
	msg = nf90_put_var(oFile, oTempSl, tempSl, (/ step /) )
end if

if (pWriteFlag(32)==1) then
	msg = nf90_put_var(oFile, ofFlex, real(fFlex,sp), (/ 1, 1, step /) )
end if

if (pWriteFlag(33)==1) then
	msg = nf90_put_var(oFile, ofSolnH, real(fSolnH,sp), (/ 1, 1, step /) )
end if

return
end subroutine writeStep

! ==================================================================================================
! closeOutput: close the output file.
! ==================================================================================================
subroutine closeOutput ()

integer msg

msg = nf90_close(oFile)
return

end subroutine closeOutput

! ==================================================================================================
! debugOut2d: Writes a single 2D array to file, used for debugging purposes
! ==================================================================================================
subroutine debugOut2d(arr,filename)

character(len=*), intent(in) :: filename
real(dp), intent(in) :: arr(:,:)

! Arguments:
!! arr (in): array to be written, must be 2D double precision
!! filename (in): string containing the output filename

integer :: nx, ny, msg, outId, xDimId, yDimId, arrVarId

nx = size(arr,1)
ny = size(arr,2)

msg = nf90_create( trim(filename), nf90_netcdf4, outId )
msg =  nf90_def_dim( outId, 'x', nx, xDimId ) 
msg =  nf90_def_dim( outId, 'y', ny, yDimId ) 
msg = nf90_def_var( outId, 'arr', nf90_double, (/ xDimId, yDimId /), arrVarId )
msg = nf90_put_var( outId, arrVarId, arr, (/ 1, 1 /) )
msg = nf90_close(outId)

return
end subroutine debugOut2d

end module io
