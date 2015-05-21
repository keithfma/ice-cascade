! =============================================================================
! Glacier dynamics model component for ice-cascade
!
! Public:
!   on_ice: logical, enable/disable model component
!   on_ice_soln: logical, enable/disable exact solution
!   init_ice: subroutine, select and intialize model procedures
!   update_ice: subroutine, run ice model for one timestep
!   solve_ice: procedure, selected exact solution procedure
!
! Private:
!   bc_tmpl: template, common form for bc procedures 
!   flow_tmpl: template, common form for numerical ice flow procedures
!   solve_tmpl: template, common form for exact solution procedures
!   sbc, nbc, wbc, ebc: procedure ptrs, selected bc procedures
!   flow: procedure ptr, selected numerical ice flow procedure
!   
! ============================================================================

module ice

use kinds, only: rp
use param, only: param_type
use state, only: state_type
use ice_bc_no_ice, only: nbc_no_ice, ebc_no_ice, sbc_no_ice, wbc_no_ice 
use ice_bc_mirror, only: nbc_mirror, ebc_mirror, sbc_mirror, wbc_mirror 
use ice_flow_hindmarsh2_explicit, only: &
  init_hindmarsh2_explicit, flow_hindmarsh2_explicit
use ice_soln_bueler_isothermal_a, only: &
  init_bueler_isothermal_a, solve_bueler_isothermal_a
use ice_soln_bueler_isothermal_b, only: &
  init_bueler_isothermal_b, solve_bueler_isothermal_b

implicit none
private
public :: on_ice, on_ice_soln, init_ice, solve_ice, update_ice


  ! ---------------------------------------------------------------------------
  ! TEMPLATE: common form for the bc functions
  ! ---------------------------------------------------------------------------
  abstract interface 
    subroutine bc_tmpl(s)
      import :: state_type ! use special types
      type(state_type), intent(inout) :: s   
    end subroutine bc_tmpl 
  end interface


  ! ---------------------------------------------------------------------------
  ! TEMPLATE: common form for the numerical ice flow routine
  !
  !   Description: flow routines compute a numerical approximation to the ice
  !     flow equations, including sliding, temperature, and hydrology, where
  !     applicable. They MUST set the value of the ice_h_dot variable, and may
  !     set the value of other state variables as well.
  ! ---------------------------------------------------------------------------
  abstract interface 
    function flow_tmpl(p, s) result(dt)
      import :: rp, param_type, state_type ! use special types
      type(param_type), intent(in) :: p          ! parameters
      type(state_type), intent(inout) :: s       ! state vars
      real(rp) :: dt ! timestep
    end function flow_tmpl 
  end interface


  ! ---------------------------------------------------------------------------
  ! TEMPLATE: common form for the exact solution subroutines
  ! ---------------------------------------------------------------------------
  abstract interface 
    subroutine solve_tmpl(p, s)
      import :: param_type, state_type     ! use special types
      type(param_type), intent(in)    :: p ! parameters
      type(state_type), intent(inout) :: s ! state vars
    end subroutine solve_tmpl 
  end interface


  ! ---------------------------------------------------------------------------
  ! PROCEDURES & VARS: set in init_ice 
  ! ---------------------------------------------------------------------------
  logical :: on_ice ! enable/disable model
  logical :: on_ice_soln ! enable/disable exact solution
  procedure(bc_tmpl), pointer :: nbc ! apply north BC, sets (:,end)
  procedure(bc_tmpl), pointer :: sbc ! apply south BC, sets (:,1)
  procedure(bc_tmpl), pointer :: ebc ! apply east BC, sets (end,:)
  procedure(bc_tmpl), pointer :: wbc ! apply west BC, sets (1,:)
  procedure(flow_tmpl), pointer :: flow ! ice model method
  procedure(solve_tmpl), pointer :: solve_ice ! exact solution  


  ! ---------------------------------------------------------------------------
  ! VARS: global variables
  ! ---------------------------------------------------------------------------


contains


  ! ---------------------------------------------------------------------------
  ! SUB: initialize procedures and vars
  ! ---------------------------------------------------------------------------
  subroutine init_ice(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s

    character(len=100), dimension(4) :: bc_names
    integer :: comma(3), i

    ! select BC procedures
    read(p%ice_bc_name,*) bc_names ! n,e,s,w

    do i = 1, 4
      select case (bc_names(i))

        case ('none') ! will seg-fault if called, by design 
          if (i .eq. 1) nbc => NULL() 
          if (i .eq. 2) ebc => NULL() 
          if (i .eq. 3) sbc => NULL() 
          if (i .eq. 4) wbc => NULL() 

        case ('no_ice')
          if (i .eq. 1) nbc => nbc_no_ice 
          if (i .eq. 2) ebc => ebc_no_ice
          if (i .eq. 3) sbc => sbc_no_ice
          if (i .eq. 4) wbc => wbc_no_ice

        case ('mirror')
          if (i .eq. 1) nbc => nbc_mirror 
          if (i .eq. 2) ebc => ebc_mirror
          if (i .eq. 3) sbc => sbc_mirror
          if (i .eq. 4) wbc => wbc_mirror

        case default
          print *, "Invalid name for boundary condition: " // trim(bc_names(i))
          stop 

      end select
    end do

    ! select flow procedure
    select case (p%ice_name)

      case ('none') 
        on_ice = .false. 
        flow => NULL() ! will seg-fault if called, by design

      case ('hindmarsh2_explicit')
        on_ice = .true.
        call init_hindmarsh2_explicit(p, s)
        flow => flow_hindmarsh2_explicit 

      case default
        print *, "Invalid name for glacier flow method: " // trim(p%ice_name)
        stop 

    end select

    ! select exact solution procedure
    select case (p%ice_soln_name)

      case ('none') 
        on_ice_soln = .false. 
        solve_ice => NULL() ! will seg-fault if called, by design

      case('bueler_isothermal_a')
        on_ice_soln = .true.
        call init_bueler_isothermal_a(p, s)
        solve_ice => solve_bueler_isothermal_a

      case('bueler_isothermal_b')
        on_ice_soln = .true.
        call init_bueler_isothermal_b(p, s)
        solve_ice => solve_bueler_isothermal_b

      case default
        print *, "Invalid name for glacier exact solution: " // trim(p%ice_soln_name)
        stop 

    end select

  end subroutine init_ice


  ! ---------------------------------------------------------------------------
  ! SUB: run ice model for one timestep
  ! ---------------------------------------------------------------------------
  subroutine update_ice(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s

    integer :: i, j ! debug
    real(rp) :: t, dt

    t = 0.0_rp
    do while (t .lt. p%time_step) 

      ! init grids
      call nbc(s)
      call ebc(s)
      call sbc(s)
      call wbc(s)
      s%surf = s%topo+s%ice_h

      ! ice flow procedure
      dt = flow(p, s)
      dt = min(dt, p%time_step-t) ! trim last step

      ! update
      t = t+dt
      s%ice_h = s%ice_h+dt*(s%ice_h_dot+s%ice_q_surf)
      s%ice_h = max(s%ice_h, 0.0_rp)

    end do
    
    ! update ancillary variables (velocity, etc.)
    ! NOTE: this will require a separate procedure for each flow method.

  end subroutine update_ice


end module ice
