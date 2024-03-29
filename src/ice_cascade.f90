program ice_cascade

! Numerical model of glacial, fluvial, and hillslope erosion using modified versions of the ICE and
!! CASCADE models described in Braun & Sambridge, 1997, Basin Research; Braun et al, 1999, Annals of 
!! Glaciology; Herman et al, 2008, JGR; and others. The models are implemented in separate modules
!! called by this main program.
! 
! "Hungarian" prefixes are used to indicate the inteded usage of the variable:
!! p -> input parameter
!! f -> full-resolution grid
!! l -> low-resolution grid, include a 1-point pad on all sides used for boundary conditions

! Version info (auto-generated by svn):
!! $LastChangedDate: 2014-05-10 22:44:34 -0400 (Sat, 10 May 2014) $
!! $LastChangedBy: kfm $
!! $LastChangedRevision: 100 $ 

use types, only: dp, dp_mpi, dp_eps
use mpi, only: mpi_init, mpi_comm_rank, mpi_comm_world, mpi_finalize
use cascade, only: runCascade
use io, only: readParams, initGrids, createOutput, writeConst, writeStep, closeOutput, debugOut2d
use ice, only: runIce
use hillslope, only: hillslopeDiffusion
use flexure, only: flexInit, flex
use uplift, only: upliftTri2, upliftBlock1
use benchmark, only: benchSolution
implicit none

character(len=150) :: pRunName, pTopoFile, pIceFile
logical :: pDoGlac, pDoFluv, pDoHill, pDoUplift, pDoEros, pDoFlex, pDoTemp, pDoTrackIceVol, &
	pDoAddNoise, pDoPrefilter
logical, allocatable :: fIceFree(:,:), fLake(:,:)
integer :: ierr, proc, pBenchmark, lNx, fNx, lNy, fNy, pWriteFreq, step, outputStep, count0, &
	countRate, pGlacBcN, pGlacBcS, pGlacBcE, pGlacBcW, pFluvBcN, pFluvBcS, pFluvBcE, pFluvBcW, &
	pNxPad, pNyPad, pHillBcN, pHillBcS, pHillBcE, pHillBcW
integer, allocatable :: pWriteFlag(:), fCatchment(:,:)
real(dp) :: lDx, fDx, lDy, fDy, pTimeEnd, pTimeStepIceMax, pTimeStepIceMin, pTimeStep, pB, pBs, pGamma, pQheatb, &
	pHeatConduct, pTempSlMin, pTempSlMax, pTempPeriod, pTempLapse, pTempYrRng, pMeltFact, &
	pPrecipRate, pRhoIce, pRhoWater, pRhoCrust, pRhoMantle, pYm, pNu, pTe, pGlacErosFact, &
	pFluvErosFact, pHillD, pUpliftRate, pGlacErosRateCeil, pTFloor, pTempDiffuse, pTempBasalGrad, &
	pC, pCs, dvolIceSrc, dvolIceSnk, dvolIce, time, timeStepIceMean, tempSl, pBaseLvl
real(dp), allocatable :: fX(:,:), fY(:,:), fT(:,:), fH(:,:), fHT(:,:), fTInit(:,:), fHInit(:,:), &
	fSliding(:,:), fUpliftRate(:,:), fGlacErosRate(:,:), fHillErosRate(:,:), fFluvErosRate(:,:), &
	fSlope(:,:), fSolnH(:,:), lX(:,:), lY(:,:), lT(:,:), lH(:,:), lHT(:,:), lTempS(:,:), &
	lTempB(:,:), lTempM(:,:), lBalRate(:,:), lUDefm(:,:), lVDefm(:,:), lUSlid(:,:), lVSlid(:,:), &
	lSliding(:,:), lConstrict(:,:), fQWater(:,:), fWater(:,:), fFlex(:,:)

! Start MPI
call mpi_init( ierr )
call mpi_comm_rank( mpi_comm_world, proc, ierr )

! Read model parameters
call readParams( pBenchmark, pRunName, pTopoFile, pIceFile, lNx, fNx, lNy, fNy, lDx, fDx, lDy, &
	fDy, pDoEros, pDoFlex, pDoTemp, pTimeEnd, pTimeStepIceMax, pTimeStepIceMin, pTimeStep, &
	pWriteFreq, pB, pBs, pGamma, pQheatb, pHeatConduct, pTempSlMin, pTempSlMax, pTempPeriod, &
	pTempLapse, pTempYrRng, pMeltFact, pPrecipRate, pRhoIce, pRhoWater, pRhoCrust, pRhoMantle, &
	pYm, pNu, pTe, pGlacErosFact, pFluvErosFact, pHillD, pUpliftRate, pGlacErosRateCeil, pTFloor, &
	pTempDiffuse, pTempBasalGrad, pC, pCs, pGlacBcN, pGlacBcS, pGlacBcE, pGlacBcW, pDoGlac, &
	pDoFluv, pDoHill, pDoUplift, pDoTrackIceVol, pDoAddNoise, pDoPrefilter, pFluvBcN, pFluvBcS, &
	pFluvBcE, pFluvBcW, pBaseLvl, pNxPad, pNyPad, pHillBcN, pHillBcS, pHillBcE, pHillBcW, pWriteFlag )
	
! Allocate arrays
!! High-res
allocate( fX(fNx,fNy), fY(fNx,fNy), fT(fNx,fNy), fH(fNx,fNy), fHT(fNx,fNy), fTInit(fNx,fNy), &
	fHInit(fNx,fNy), fSliding(fNx,fNy), fUpliftRate(fNx,fNy), fGlacErosRate(fNx,fNy), &
	fIceFree(fNx,fNy), fHillErosRate(fNx,fNy), fSlope(fNx,fNy), fFluvErosRate(fNx,fNy), &
	fSolnH(fNx,fNy), fQWater(fNx,fNy), fWater(fNx,fNy), fLake(fNx,fNy), fCatchment(fNx,fNy), &
	fFlex(fNx,fNy) )
!! Low-res
allocate( lX(lNx,lNy), lY(lNx,lNy), lT(lNx,lNy), lH(lNx,lNy), lHT(lNx,lNy), lTempS(lNx,lNy), &
	lTempB(lNx,lNy), lTempM(lNx,lNy), lBalRate(lNx,lNy), lUDefm(lNx,lNy), lVDefm(lNx,lNy), &
	lUSlid(lNx,lNy), lVSlid(lNx,lNy), lSliding(lNx,lNy), lConstrict(lNx,lNy) )

! Populate grids with initial values
call initGrids( pTopoFile, pIceFile, pBenchmark, lT, fT, lH, fH, lDx, lDy, fDx, fDy, lX, &
	fX, lY, fY, pDoAddNoise, pDoPrefilter, pUpliftRate, pB, pRhoIce )

! Create output file
if (proc==0) &
	call createOutput ( pRunName, pTopoFile, pIceFile, lNx, fNx, lNy, fNy, lDx, fDx, lDy, fDy, &
		pDoEros, pDoFlex, pDoTemp, pTimeEnd, pTimeStepIceMax, pTimeStepIceMin, pTimeStep, pB, pBs, &
		pGamma, pQheatb, pHeatConduct, pTempSlMin, pTempSlMax, pTempPeriod, pTempLapse, &
		pTempYrRng, pMeltFact, pPrecipRate, pRhoIce, pRhoWater, pRhoCrust, pRhoMantle, pYm, pNu, &
		pTe, pGlacErosFact, pHillD, pUpliftRate, pGlacErosRateCeil, pTFloor, pGlacBcN, pGlacBcS, &
		pGlacBcE, pGlacBcW, pDoGlac, pDoFluv, pDoHill, pDoUplift, pDoTrackIceVol, pDoAddNoise, &
		pDoPrefilter, pFluvBcN, pFluvBcS, pFluvBcE, pFluvBcW, pFluvErosFact, pBaseLvl, pNxPad, &
		pNyPad, pHillBcN, pHillBcS, pHillBcE, pHillBcW, pWriteFlag )

! Compute constant variables
if (pDoUplift) call upliftTri2 (pUpliftRate, fUpliftRate)

! Write initial values to output
if (proc.eq.0) then 	
	!! Note: where exact steady state benchmark solutions exists, they are used as the initial fH
	call writeConst (fUpliftRate, fH, pWriteFlag)
	call writeStep ( 1, 0._dp, pWriteFlag, lH, fH, lT, fT, lHT, fHT, lTempS, lTempB, lTempM, &
		lUDefm, lVDefm, lUSlid, lVSlid, lSliding, fSliding, lConstrict, fGlacErosRate, &
		fHillErosRate, fSlope, fQWater, fFluvErosRate, fWater, dvolIceSrc, dvolIceSnk, dvolIce, &
		lBalRate, fLake, fCatchment, 0._dp, 0._dp, fFlex, fH)
end if

! Setup flexure computations
if (pDoFlex) call flexInit(pNxPad,pNyPad)

! Setup main loop
step = 0
outputStep = 1
time = 0.
!! Setup clock
call system_clock (count0, countRate) 

! Enter main loop
do while (time.lt.pTimeEnd)

	!! Store initial values for flexure load 
	if (pDoFlex) then
		fHInit = fH
		fTInit = fT
	end if
	
	!! Adjust time step if needed
	if ( (time+pTimeStep)>pTimeEnd) pTimeStep = pTimeEnd-time	
	
	!! Ice model
	if (pDoGlac) then
		call runIce ( time, time+pTimeStep, pTimeStepIceMax, pTimeStepIceMin, pB, pC, pCs, pGamma, &
			pDoTemp, pTempBasalGrad, pTempDiffuse, pTempSlMin, pTempSlMax, pTempPeriod, &
			pTempLapse, pPrecipRate, pTempYrRng, pMeltFact, pRhoIce, pRhoWater, pBenchmark, lX, &
			lY, lDx, lDy, fX, fY, fDx, fDy, fT, lH, lHT, fH, fHT, lTempS, lTempB, lTempM, &
			lBalRate, lT, lUDefm, lVDefm, lUSlid, lVSlid, lSliding, fSliding, lConstrict, &
			dvolIceSrc, dvolIceSnk, dvolIce, pGlacBcN, pGlacBcS, pGlacBcE, pGlacBcW, &
			pDoTrackIceVol, fIceFree, fGlacErosRate, pGlacErosRateCeil, pGlacErosFact, fWater, &
			timeStepIceMean, tempSl )
	else
		fIceFree = .true.
	end if
		
	!! Precipitation & melt (initial water discharge -> fQWater)
	!!! Effective precip rate is generated by the ice model
	if (pDoGlac.and.pDoFluv) then 
		fQWater = fWater*lDx*lDy
	!!! Precipitation only, assumes all snow melts 
	else if (pDoFluv) then 
		fWater = pPrecipRate
		fQWater = pPrecipRate*lDx*lDy 
	end if
	
	!! Fluvial model
	if (pDoFluv) then		
		ierr = 0
		call runCascade( fT, fDx, fDy, pFluvBcN, pFluvBcS, pFluvBcE, pFluvBcW, pFluvErosFact, &
			pBaseLvl, fIceFree, fQWater, fSlope, fLake, fCatchment, fFluvErosRate, ierr )			
		!!! DEBUG cascade
		if (ierr/=0) then
			call debugOut2d(real(fCatchment,dp),'errCatchment.nc')
			call debugOut2d(fT,'errTopo.nc')
			stop
		end if
	end if
			
	!! Hillslope model
	if (pDoHill) &
		call hillslopeDiffusion ( fT, fDx, fDy, pHillD, pHillBcN, pHillBcS, pHillBcE, pHillBcW, &
			fIceFree, fHillErosRate )
	
	!! Uplift
	if (pDoUplift) then		
		fT = fT + fUpliftRate*pTimeStep
	end if
	
	!! Erode		
	if (pDoEros) then		
		fT = fT - (fFluvErosRate + fGlacErosRate + fHillErosRate)*pTimeStep
		where (fT<pTFloor) fT = pTFloor	
	end if
	
	!! Flex
	if (pDoFlex) then
		call flex( fT-fTInit, fH-fHInit, fDx, fDy, pRhoCrust, pRhoIce, pRhoMantle, pYm, pNu, &
			pTe, fFlex )
		fT = fT-fFlex
	end if	
			
	!! Step completed
	time = time + pTimeStep
	step = step + 1
	
	!! Benchmark solution, time-dependant
	if (pBenchmark/=0) call benchSolution( pBenchmark, fX, fY, pB, pRhoIce, time, fSolnH )
	
	!! Write output
	if ( ((step/pWriteFreq*pWriteFreq==step).or.(time==pTimeEnd)) .and. (proc==0) ) then
		outputStep = outputStep + 1			
		call writeStep ( outputStep, time, pWriteFlag, lH, fH, lT, fT, lHT, fHT, lTempS, lTempB, &
			lTempM, lUDefm, lVDefm, lUSlid, lVSlid, lSliding, fSliding, lConstrict, fGlacErosRate, &
			fHillErosRate, fSlope, fQWater, fFluvErosRate, fWater, dvolIceSrc, dvolIceSnk, &
			dvolIce, lBalRate, fLake, fCatchment, timeStepIceMean, tempSl, fFlex, fSolnH)
	end if
		
end do
! Exit main loop		

! End program
if (proc.eq.0) call closeOutput()
call mpi_finalize(ierr)

stop
end program ice_cascade
