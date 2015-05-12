program ice_cascade

use kinds_mod, only: rp
use param_mod, only: param_type
use state_mod, only: state_type
use climate_mod, only: climate_type
use ice_mod, only: ice_type
use io_mod, only: read_param, read_var, write_file, write_step, write_status

implicit none

type(param_type) :: prm
type(state_type) :: sta
type(climate_type) :: cli
type(ice_type) :: ice

! Initialize
call read_param(prm)
call prm%init() 
call sta%init(prm)
call cli%init(prm)
call ice%init(prm)
call read_var(prm, sta)
call write_file(prm)

! Start loop
sta%time_now = prm%time_start
do while (sta%time_now .lt. prm%time_finish)

  ! Update model state 
  if (cli%on) call cli%update(prm, sta)
  if (ice%on) call ice%update(prm, sta)

  ! Increment time
  sta%time_now = sta%time_now+prm%time_step

  ! Apply erosion/deposition/isostasy

  ! Write step 
  if (mod(sta%time_now-prm%time_start, prm%time_step) .eq. 0.0_rp) then
    if (ice%on_soln) call ice%solve(prm, sta)
    call write_status(prm, sta)
    call write_step(prm, sta)
  end if

end do
! End loop

end program ice_cascade
