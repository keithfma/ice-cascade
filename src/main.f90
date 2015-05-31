program ice_cascade

use kinds, only: rp
use param, only: param_type, init_param
use state, only: state_type, init_state
use io, only: read_param, read_var, write_file, write_step, write_status
use climate, only: on_climate, init_climate, update_climate
use ice, only: on_ice, on_ice_soln, init_ice, solve_ice, update_ice

implicit none

type(param_type) :: p
type(state_type) :: s
real(rp) :: step_lim
integer :: n

! NOTE: should the init routines run the update functions to set the starting
! values for non-specified vars?

! NOTE: will have to adjust init_state and the io routines to create arrays with
! ghost points

! read input data and init data objects
call read_param(p)
call init_param(p) 
call init_state(p, s)
call read_var(p, s)

! init module components
call init_ice(p, s) 
call init_climate(p, s)

! init output file
call write_file(p, s)

! Start loop
n = 1
do while (s%now .lt. p%time_finish)

  ! truncate time step to hit end or write time exactly
  step_lim = min(p%time_write(n), p%time_finish)-s%now
  if (step_lim .lt. s%step) s%step = step_lim

  ! Increment time
  s%now = s%now+s%step

  ! Update model state 
  if (on_climate) call update_climate(p, s)
  if (on_ice) call update_ice(p, s)

  ! Apply erosion/deposition/isostasy

  ! Write step 
  if (abs(s%now-p%time_write(n)) .le. epsilon(0.0_rp)) then
    s%step = p%time_step ! reset step
    if (on_ice_soln) call solve_ice(p, s)
    call write_status(p, s)
    call write_step(p, s)
    n = n+1
  end if

end do
! End loop

end program ice_cascade
