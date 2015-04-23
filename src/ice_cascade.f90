program ice_cascade

use kinds_mod, only: rp
use common_mod, only: common_type
use time_mod, only: time_type
use climate_mod, only: climate_type
use io_mod, only: io_type

implicit none

type(io_type) :: io 
type(time_type) :: t 
type(common_type) :: c 
type(climate_type) :: cl

! Initialize
call io%read_param(t, c, cl)
call t%check()
call c%check(); call c%init()
call cl%init()
call io%read_initial_vals(c)

! Get model state at time = start

! Create output file and write initial values
call io%create_output(t, c, cl)

! Start loop
do while (t%now .le. t%finish-t%step)

  ! Truncate last timestep, if needed

  ! Update model state (climate, ice, time, other)
  t%now = t%now+t%step
  call cl%update(c)


  ! Apply erosion/deposition/isostasy

  ! Write timestep
  if (mod(t%now-t%start, t%step_out) .eq. 0.0_rp) then
    call io%write_status(t, c)
    call io%write_output_step(t, c)
  end if

end do
! End loop

end program ice_cascade
