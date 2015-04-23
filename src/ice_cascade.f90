program ice_cascade

use kinds_mod, only: rp
use common_mod, only: common_type
use time_mod, only: time_type
use io_mod, only: io_type

implicit none

type(io_type) :: w ! output flags
type(time_type) :: t ! time vars
type(common_type) :: c ! common vars

! Initialize
call w%read_param(t, c)
call w%init()
call t%check()
call c%check(); call c%init()
call w%read_initial_vals(c)

! Get model state at time = start

! Create output file and write initial values
call w%create_output(t, c)

! Start loop
do while (t%now .le. t%finish)

  ! Truncate last timestep, if needed

  ! Update model state (climate, ice, time, other)
  t%now = t%now+t%step

  ! Compute erosion/deposition/isostasy

  ! Apply erosion/deposition/isostasy

  ! Write timestep
  if (mod(t%now-t%start, t%step_out) .eq. 0.0_rp) then
    call w%write_status(t, c)
    call w%write_output_step(t, c)
  end if

end do
! End loop

end program ice_cascade
