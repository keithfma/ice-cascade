program ice_cascade

use kinds_mod, only: rp
use param_mod, only: param_type
use state_mod, only: state_type
use climate_mod, only: climate_type
use ice_mod, only: ice_type
use io_mod, only: io_type

implicit none

type(io_type) :: io 
type(param_type) :: prm
type(state_type) :: sta
type(climate_type) :: cli
type(ice_type) :: ice

! Initialize objects
call io%read_param(prm)
call prm%check() 
call sta%alloc(prm%nx+2, prm%ny+2)
call io%read_var(prm, sta)

! Update model at time = start
sta%time_now = prm%time_start

! Create output file and write initial values
call io%write_file(prm)
call io%write_step(prm, sta)

! Start loop
do while (sta%time_now .lt. prm%time_finish)

  ! Update model state (climate, ice, time, other)
  sta%time_now = sta%time_now+prm%time_step
!  call c%update(s)
!  call g%update(s)

!  ! Apply erosion/deposition/isostasy

  ! Write timestep
  if (mod(sta%time_now-prm%time_start, prm%time_step) .eq. 0.0_rp) then
!    if (g%on_soln) call g%solve(s)
    call io%write_status(prm, sta)
    call io%write_step(prm, sta)
  end if

end do
! End loop

end program ice_cascade
