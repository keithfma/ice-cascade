program ice_cascade

use kinds_mod, only: rp
use state_mod, only: state_type
use climate_mod, only: climate_type
use io_mod, only: io_type

implicit none

type(io_type) :: io 
type(state_type) :: s 
type(climate_type) :: c

! Initialize
call io%read_param(s, c)
call s%init()
call c%init()
call io%read_initial_vals(s)

! Get model state at time = start

! Create output file and write initial values
call io%create_output(s, c)

! Start loop
do while (s%time_now .le. s%time_finish-s%time_step)

  ! Truncate last timestep, if needed

  ! Update model state (climate, ice, time, other)
  s%time_now = s%time_now+s%time_step
  call c%update(s)


  ! Apply erosion/deposition/isostasy

  ! Write timestep
  if (mod(s%time_now-s%time_start, io%time_step) .eq. 0.0_rp) then
    call io%write_status(s)
    call io%write_output_step(s)
  end if

end do
! End loop

end program ice_cascade
