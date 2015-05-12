! =============================================================================
! Climate model, including variables and procedures
!
! Contains:
!   type climate_type (public)
!   NOTE: list is not yet complete
! ============================================================================

module climate_mod

use kinds_mod, only: rp
use param_mod, only: param_type
use state_mod, only: state_type

implicit none
private
public climate_type

  ! --------------------------------------------------------------------------- 
  ! TYPE: all variables and procedures for the climate model component
  ! ---------------------------------------------------------------------------
  type climate_type
    logical                                 :: on     ! enable/disable
    procedure(update_tmpl), pointer, nopass :: update ! selected climate method
  contains
    procedure, pass                         :: init   ! initialize model object
  end type climate_type


  ! ---------------------------------------------------------------------------
  ! TEMPLATE: commom form for climate methods
  ! ---------------------------------------------------------------------------
  abstract interface
    subroutine update_tmpl(p, s) 
      import                          :: param_type, state_type
      type(param_type), intent(in)    :: p 
      type(state_type), intent(inout) :: s
    end subroutine update_tmpl
  end interface

contains


! INITIALIZE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! ---------------------------------------------------------------------------
  ! SUB: Initilialize climate object
  ! ---------------------------------------------------------------------------
  subroutine init(c, p)

    class(climate_type), intent(inout) :: c
    type(param_type), intent(in) :: p

    ! assign climate method
    select case (p%climate_name)

    case ('none')
      c%on = .false.
      c%update => NULL() ! will seg-fault if called, by design

    case('bueler_isothermal_a')
      call set_bueler_isothermal_a(c, p)

    case default
      print *, 'Invalid parameter: climate method name is not recognized.'
      stop 'Stopped.'

    end select

  end subroutine init


! METHODS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  ! ---------------------------------------------------------------------------
  ! Bueler et al 2005 test A methods. Enabled if climate_name is 
  !   'bueler_isothermal_a'. 
  !
  ! Parameters:
  !   (1) M0: constant positive balance rate within the ice cap
  !   (2) L: constant fixed radius of the ice cap 
  !
  ! References:
  ! [1] Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman, L.
  ! N. (2005). Exact solutions and verification of numerical models for isothermal
  ! ice sheets. Journal of Glaciology, 51(173), 291-306.
  ! doi:10.3189/172756505781829449
  ! ---------------------------------------------------------------------------

  subroutine set_bueler_isothermal_a(c, p)

    type(climate_type), intent(inout) :: c
    type(param_type), intent(in) :: p

    c%on = .true.
    c%update => update_bueler_isothermal_a

    ! M0 must be positive
    if (p%climate_param(1) .le. 0.0_rp) then
      print *, 'Invalid climate parameter: method requires a positive M0'
      stop
    end if

    ! L must be positive
    if (p%climate_param(2) .le. 0.0_rp) then
      print *, 'Invalid climate parameter: method requires a positive L'
      stop 
    end if

  end subroutine set_bueler_isothermal_a


  subroutine update_bueler_isothermal_a(p, s)

    type(param_type), intent(in)    :: p 
    type(state_type), intent(inout) :: s

    ! saved vars (init once)
    logical, save :: set ! flag indicating if param have been set
    real(rp), save :: M0 ! constant positive surface ice flux [m_ice/a]
    real(rp), save :: L ! fixed ice cap radius [m]
    real(rp), save :: Mn ! arbitrarily large negative iceflux outside icecap 
    real(rp), allocatable, save :: r(:,:) ! distance from corner [m]

    ! unsave vars
    integer :: i, j

    ! init, first time only
    if (.not. set) then
      set = .true.
      M0 = p%climate_param(1)
      L = p%climate_param(2)
      Mn = -1000.
      allocate(r(p%nx, p%ny))
      do j = 1, p%ny
        do i = 1, p%nx
          r(i,j) = sqrt(s%x(i)*s%x(i)+s%y(j)*s%y(j))
        end do
      end do
    end if
    
    ! update
    where (r .lt. L)
      s%ice_q_surf = M0
    elsewhere
      s%ice_q_surf = Mn
    end where

  end subroutine update_bueler_isothermal_a


end module climate_mod
