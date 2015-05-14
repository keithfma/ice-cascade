! =============================================================================
! Glacier dynamics model component for ice-cascade
!
! Public:
!   init_ice: subroutine, select and intialize model procedures
!   step_ice: subroutine, run glacier model for one timestep
!   on_ice: logical, enable/disable ice model component
!
! Private:
!   bc_tmpl: template, common form for bc procedures 
!   flow_tmpl: template, common form for numerical ice flow procedures
!   solve_tmpl: template, common form for exact solution procedures
!   sbc, nbc, wbc, ebc: procedure ptrs, selected bc procedures
!   flow: procedure ptr, selected numerical ice flow procedure
!   solve: procedure ptr, selected exact solution procedure
!   
! ============================================================================

module ice

use kinds, only: rp
use param, only: param_type
use state, only: state_type
use ice_bc_no_ice, only: nbc_no_ice, ebc_no_ice, sbc_no_ice, wbc_no_ice 
use ice_bc_mirror, only: nbc_mirror, ebc_mirror, sbc_mirror, wbc_mirror 

implicit none
private
public :: init_ice, on_ice, on_ice_soln


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
  ! ---------------------------------------------------------------------------
  abstract interface 
    subroutine flow_tmpl(p, s)
      import :: param_type, state_type ! use special types
      type(param_type), intent(in) :: p          ! parameters
      type(state_type), intent(inout) :: s       ! state vars
    end subroutine flow_tmpl 
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
  ! PROCEDURES: pointers, set in init_ice 
  ! ---------------------------------------------------------------------------
  procedure(bc_tmpl), pointer :: nbc ! apply north BC, sets (:,end)
  procedure(bc_tmpl), pointer :: sbc ! apply south BC, sets (:,1)
  procedure(bc_tmpl), pointer :: ebc ! apply east BC, sets (end,:)
  procedure(bc_tmpl), pointer :: wbc ! apply west BC, sets (1,:)
  procedure(flow_tmpl), pointer :: flow ! ice model method
  procedure(solve_tmpl), pointer :: solve ! exact solution  


  ! ---------------------------------------------------------------------------
  ! VARS: global variables
  ! ---------------------------------------------------------------------------
  logical, save :: on_ice ! enable/disable model
  logical, save :: on_ice_soln ! enable/disable exact solution


contains


  ! ---------------------------------------------------------------------------
  ! SUB: select and initialize model procedures
  ! ---------------------------------------------------------------------------
  subroutine init_ice(p)

    type(param_type), intent(in) :: p

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
    case default
      print *, "Invalid name for glacier flow method: " // trim(p%ice_name)
      stop 
    end select

    ! select flow procedure
    select case (p%ice_soln_name)
    case ('none') 
      on_ice_soln = .false. 
      solve => NULL() ! will seg-fault if called, by design
    case default
      print *, "Invalid name for glacier exact solution: " // trim(p%ice_soln_name)
      stop 
    end select

  end subroutine init_ice


end module ice
