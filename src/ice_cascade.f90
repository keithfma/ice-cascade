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

! Start loop

  ! Truncate last timestep, if needed

  ! Update model state (climate, ice, time, other)

  ! Compute erosion/deposition/isostasy

  ! Apply erosion/deposition/isostasy

  ! Write timestep


! End loop

end program ice_cascade
