! =============================================================================
! Time variables and procedures for ICE-CASCADE model
!
! Contains:
!   type time_type (public)
!   func write_now (type-bound)
! ============================================================================

module time_module

use types, only: dp

implicit none
private
public time_type

  ! --------------------------------------------------------------------------- 
  ! TYPE: all variables and procedures for the hillslope model component
  ! ---------------------------------------------------------------------------
  type time_type
    real(dp)        :: now          ! current time [a]
    real(dp)        :: start        ! model start time [a]
    real(dp)        :: finish       ! model finish time [a]
    real(dp)        :: step         ! model time step [a]
    integer         :: now_step     ! current step [1]
    integer         :: out_step     ! current output step [1]
    integer         :: write_period ! interval between outputs [1]
   contains
    procedure, pass :: init         ! initialize components
    procedure, pass :: write_now    ! check if time to write
  end type time_type

contains


  ! --------------------------------------------------------------------------- 
  ! SUB: initialize time object
  ! --------------------------------------------------------------------------
  subroutine init(t)

    class(time_type), intent(inout) :: t ! time object to intialize

    t%now = t%start
    t%now_step = 0
    t%out_step = 0

  end subroutine init


  ! --------------------------------------------------------------------------- 
  ! FUNC: check if it is time to write output
  ! --------------------------------------------------------------------------- 
  function write_now(t) result(yesno)
    
    class(time_type), intent(inout) :: t
    logical                         :: yesno

    yesno = .false.
    if (t%now_step/t%write_period*t%write_period .eq. t%now_step) yesno = .true.
    if (t%now .eq. t%finish) yesno = .true.

  end function write_now


end module time_module
 
