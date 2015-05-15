program ice_cascade

use kinds, only: rp
use param, only: param_type, init_param
use state, only: state_type, init_state
use io, only: read_param, read_var, write_file, write_step, write_status
use climate, only: on_climate, init_climate, update_climate
use ice, only: on_ice, on_ice_soln, init_ice, solve_ice

implicit none

type(param_type) :: p
type(state_type) :: s

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
call init_ice(p, s) ! should update internally to set initial values
call init_climate(p, s)

! init output file
call write_file(p)
call write_step(p, s)

! Start loop
do while (s%time_now .lt. p%time_finish)

!  ! Update model state 
!  if (cli%on) call cli%update(prm, sta)
!  if (ice%on) call ice%update(prm, sta)

  ! Increment time
  s%time_now = s%time_now+p%time_step

  ! Apply erosion/deposition/isostasy

  ! Write step 
  if (mod(s%time_now-p%time_start, p%time_step) .eq. 0.0_rp) then
!    if (ice%on_soln) call ice%solve(prm, sta)
!    call write_status(prm, sta)
    call write_step(p, s)
  end if

end do
! End loop

end program ice_cascade
