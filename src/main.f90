program ice_cascade

use kinds, only: rp
use param, only: param_type
use state, only: state_type
!use climate_mod, only: climate_type
!use ice_mod, only: ice_type
use io, only: read_param, read_var, write_file, write_step, write_status
use ice, only: on_ice, on_ice_soln, init_ice

implicit none

type(param_type) :: prm
type(state_type) :: sta
!type(climate_type) :: cli
!type(ice_type) :: ice

! Initialize
call read_param(prm)
call prm%init() 


! DEBUG
call init_ice(prm)

call sta%init(prm%nx, prm%ny)
!call cli%init(prm)
!call ice%init(prm)
call read_var(prm, sta)
call write_file(prm)


!! Start loop
!sta%time_now = prm%time_start
!do while (sta%time_now .lt. prm%time_finish)
!
!  ! Update model state 
!  if (cli%on) call cli%update(prm, sta)
!  if (ice%on) call ice%update(prm, sta)
!
!  ! Increment time
!  sta%time_now = sta%time_now+prm%time_step
!
!  ! Apply erosion/deposition/isostasy
!
!  ! Write step 
!  if (mod(sta%time_now-prm%time_start, prm%time_step) .eq. 0.0_rp) then
!    if (ice%on_soln) call ice%solve(prm, sta)
!    call write_status(prm, sta)
!    call write_step(prm, sta)
!  end if
!
!end do
!! End loop

end program ice_cascade
