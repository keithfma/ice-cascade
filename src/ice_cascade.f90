program ice_cascade

use types, only: dp 
use grid_module, only: grid_type
use time_module, only: time_type
use topo_module, only: topo_type
use climate_module, only: climate_type
use hill_module, only: hill_type
use io_module, only: readParam, createOutfile, writeStep
implicit none

character(len=100) :: runname
type(grid_type)    :: fgrid
type(time_type)    :: time
type(topo_type)    :: ftopo
type(climate_type) :: fclimate
type(hill_type)    :: fhill

! Read model parameters
call readParam(runname, fgrid, time, ftopo, fclimate, fhill)
	
! Initialize objects
call fgrid%init()
call time%init()
call ftopo%init(fgrid)
call fclimate%init(fgrid)
if (fclimate%on) call fclimate%run(time%now, ftopo%z)
call fhill%init(fgrid)

! Create output file and write initial values
call createOutfile(runname, fgrid, time, ftopo, fclimate, fhill)
call writeStep(runname, time, ftopo, fclimate, fhill)


do while (time%now .lt. time%finish) ! main loop

	! Trim last time step if needed
	if ((time%now+time%step) .gt. time%finish) time%step = time%finish-time%now	

  ! Climate model
  if (fclimate%on) call fclimate%run(time%now, ftopo%z)
	
  ! Hillslope model
  if (fhill%on) call fhill%run(ftopo%z, time%step)

	! Step forward
	time%now = time%now+time%step
  time%now_step = time%now_step+1
	
	! Write output
  if (time%write_now()) call writeStep(runname, time, ftopo, fclimate, fhill)

end do ! exit main loop		

end program ice_cascade
