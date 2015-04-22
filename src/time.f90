! =============================================================================
! Time variables and procedures for ice-cascade. 
!
! Contains:
!   type time_type (public)
!
! ============================================================================

module time_mod

use kinds_mod, only: rp

implicit none
private
public :: time_type


  ! --------------------------------------------------------------------------- 
  ! TYPE: model time variables
  ! ---------------------------------------------------------------------------
  type time_type
    real(rp) :: now ! current time [a]
    real(rp) :: start ! start time [a]
    real(rp) :: finish ! finish time [a]
    real(rp) :: step ! time step [a]
    real(rp) :: step_out ! interval btw outputs, [a]
  contains
    procedure, pass :: init ! check for sane values
  end type time_type


contains


  ! --------------------------------------------------------------------------- 
  ! SUB: check for sane values
  ! ---------------------------------------------------------------------------
  subroutine init(time)

    class(time_type), intent(inout) :: time

    ! non-zero timestep 
    if (time%step .eq. 0.0_rp) then
      print *, 'Invalid time parameters: time step cannot be zero.'
      stop -1
    end if

    ! forward model, start before finish
    if ((time%step .gt. 0.0_rp) .and. (time%start .ge. time%finish))  then
      print *, 'Invalid time parameters: start must be before finish for & 
               &forward models.'
      stop -1
    end if

    ! reverse model, finish before start
    if ((time%step .lt. 0.0_rp) .and. (time%start .le. time%finish)) then
      print *, 'Invalid time parameters: start must be after finish for &
               &reverse models.'
      stop -1
    end if

    ! output interval matches time step
    if (mod(time%step_out, time%step) .ne. 0.0_rp) then
      print *, 'Invalid time parameters: output interval must be a multiple &
               &of the time step.'
      stop -1
    end if

  end subroutine init


end module time_mod
