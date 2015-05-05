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
call io%read_param(prm, cli, ice)
call prm%check() 
call sta%alloc(prm%nx, prm%ny)
!call c%init()
!call g%init(s)
!call io%read_initial_vals(s, g)
!
!! Get model state at time = start
!
!! Create output file and write initial values
!call io%create_output(s, c, g)
!
!! Start loop
!do while (s%time_now .le. s%time_finish-s%time_step)
!
!  ! Truncate last timestep, if needed
!
!  ! Update model state (climate, ice, time, other)
!  s%time_now = s%time_now+s%time_step
!  call c%update(s)
!  call g%update(s)
!
!  ! Apply erosion/deposition/isostasy
!
!  ! Write timestep
!  if (mod(s%time_now-s%time_start, io%time_step) .eq. 0.0_rp) then
!    if (g%on_soln) call g%solve(s)
!    call io%write_status(s)
!    call io%write_output_step(s)
!  end if
!
!end do
!! End loop

end program ice_cascade
