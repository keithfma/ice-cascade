program ice_cascade

use kinds_mod, only: rp
use common_mod, only: common_type
use time_mod, only: time_type
use climate_mod, only: climate_type
use io_mod, only: io_type

implicit none

type(io_type) :: io 
type(common_type) :: c 
type(climate_type) :: cl

! Initialize
call io%read_param(c, cl)
call c%init()
call cl%init()
call io%read_initial_vals(c)

! Get model state at time = start

! Create output file and write initial values
call io%create_output(c, cl)

! Start loop
do while (c%time_now .le. c%time_finish-c%time_step)

  ! Truncate last timestep, if needed

  ! Update model state (climate, ice, time, other)
  c%time_now = c%time_now+c%time_step
  call cl%update(c)


  ! Apply erosion/deposition/isostasy

  ! Write timestep
  if (mod(c%time_now-c%time_start, io%time_step) .eq. 0.0_rp) then
    call io%write_status(c)
    call io%write_output_step(c)
  end if

end do
! End loop

end program ice_cascade
